{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries(getDayOfWeather, logWeather) where

import qualified Data.Time as Time
import qualified Data.ByteString.Char8 as ByteString
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite3

import qualified Args
import qualified Types
import Types
  (Location(..),
   Weather(..),
   created,
   temperature,
   humidity,
   location)


weatherQuery
  :: Sqlite3.Connection
  -> String
  -> [HDBC.SqlValue]
  -> IO (Maybe [Types.Weather])
weatherQuery connection query args = sequenceA . map readWeatherRow
  <$> HDBC.quickQuery' connection query args

selectByDateRange
  :: Sqlite3.Connection
  -> Time.UTCTime
  -> Time.UTCTime
  -> IO (Maybe [Weather])
selectByDateRange connection from to = print (from, to) >> weatherQuery
  connection
  "select created, humidity, temperature, location from weather where created >= ? and created < ?"
  [serializeTime from, serializeTime to]

insert :: Sqlite3.Connection -> Weather -> IO Integer
insert connection weather = HDBC.run
  connection
  "insert into weather (created, humidity, temperature, location) values (?, ?, ?, ?)"
  (serializeWeather weather)

serializeTime :: Time.UTCTime -> HDBC.SqlValue
serializeTime = HDBC.SqlInt64
  . read
  . Time.formatTime Time.defaultTimeLocale "%s"

serializeWeather :: Weather -> [HDBC.SqlValue]
serializeWeather (Weather { created, humidity, temperature, location }) =
  [serializeTime created,
   HDBC.SqlInt64 (fromIntegral humidity),
   HDBC.SqlInt64 (fromIntegral temperature),
   serializeLocation location]

serializeLocation :: Types.Location -> HDBC.SqlValue
serializeLocation Inside = HDBC.SqlByteString "inside"
serializeLocation Outside = HDBC.SqlByteString "outside"

readLocation :: HDBC.SqlValue -> Maybe Types.Location
readLocation (HDBC.SqlByteString "inside") = Just Inside
readLocation (HDBC.SqlByteString "outside") = Just Outside
readLocation _ = Nothing

readCreated :: HDBC.SqlValue -> Maybe Time.UTCTime
readCreated (HDBC.SqlInt64 time) = Time.parseTimeM
  True
  Time.defaultTimeLocale
  "%s"
  (show time)
readCreated _ = Nothing

readSqlInt64 :: HDBC.SqlValue -> Maybe Int
readSqlInt64 (HDBC.SqlInt64 x) = Just (fromIntegral x)
readSqlInt64 _ = Nothing

readWeatherRow :: [HDBC.SqlValue] -> Maybe Weather
readWeatherRow [created, humidity, temperature, location] = do
  created' <- readCreated created
  location' <- readLocation location
  humidity' <- readSqlInt64 humidity
  temperature' <- readSqlInt64 temperature
  pure Weather
    { created = created'
    , location = location'
    , temperature = temperature'
    , humidity = humidity'
    }
readWeatherRow _ = Nothing

minusOneDay :: Time.UTCTime -> Time.UTCTime
minusOneDay time = Time.addUTCTime (-1 * 60 * 60 * 24) time

get24HoursWeatherRows :: Sqlite3.Connection -> IO (Maybe [Types.Weather])
get24HoursWeatherRows connection = do
  now <- Time.getCurrentTime
  selectByDateRange connection (minusOneDay now) now

getCurrentWeather :: Types.Location -> IO Types.Weather
getCurrentWeather location = do
  (temperature, humidity) <- Args.getTemperatureHumidity
  created <- Time.getCurrentTime
  pure (Types.Weather {temperature, humidity, created, location})

getAndInsertCurrentWeather :: Sqlite3.Connection -> Types.Location -> IO ()
getAndInsertCurrentWeather connection location = do
  weather <- getCurrentWeather location
  _ <- insert connection weather
  pure ()

withWeatherTransaction :: (Sqlite3.Connection -> IO ()) -> IO ()
withWeatherTransaction f = Sqlite3.connectSqlite3 "weather.db" >>=
  (`HDBC.withTransaction` f)

logWeather :: IO ()
logWeather = do
  connection <- Sqlite3.connectSqlite3 "weather.db"
  getAndInsertCurrentWeather connection Inside
  HDBC.commit connection

getDayOfWeather :: IO (Maybe [Types.Weather])
getDayOfWeather = do
  connection <- Sqlite3.connectSqlite3 "weather.db"
  get24HoursWeatherRows connection
