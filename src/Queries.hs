{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries(getDayOfWeather, logWeather) where

import qualified Data.Time             as Time
import qualified Database.HDBC         as HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite3

import           Types                 (Location (..), Weather (..), created,
                                        humidity, location, temperature)


weatherQuery
  :: Sqlite3.Connection
  -> String
  -> [HDBC.SqlValue]
  -> IO (Maybe [Weather])
weatherQuery connection query args = sequenceA . map readWeatherRow
  <$> HDBC.quickQuery' connection query args

selectByDateRange
  :: Sqlite3.Connection
  -> Time.UTCTime
  -> Time.UTCTime
  -> IO (Maybe [Weather])
selectByDateRange connection from to = weatherQuery
  connection
  (unlines ["select created, humidity, temperature, location from weather",
            "where created >= ? and created < ?",
            "order by created"])
  [serializeTime from, serializeTime to]

insert :: Sqlite3.Connection -> Weather -> IO Integer
insert connection (Weather { created, humidity, temperature, location }) = HDBC.run
  connection
  "insert into weather (created, humidity, temperature, location) values (?, ?, ?, ?)"
  [serializeTime created,
   HDBC.SqlInt64 (fromIntegral humidity),
   HDBC.SqlInt64 (fromIntegral temperature),
   serializeLocation location]

serializeTime :: Time.UTCTime -> HDBC.SqlValue
serializeTime = HDBC.SqlInt64
  . read
  . Time.formatTime Time.defaultTimeLocale "%s"

serializeLocation :: Location -> HDBC.SqlValue
serializeLocation Inside  = HDBC.SqlByteString "inside"
serializeLocation Outside = HDBC.SqlByteString "outside"

readLocation :: HDBC.SqlValue -> Maybe Location
readLocation (HDBC.SqlByteString "inside")  = Just Inside
readLocation (HDBC.SqlByteString "outside") = Just Outside
readLocation _                              = Nothing

readCreated :: HDBC.SqlValue -> Maybe Time.UTCTime
readCreated (HDBC.SqlInt64 time) = Time.parseTimeM
  True
  Time.defaultTimeLocale
  "%s"
  (show time)
readCreated _ = Nothing

readSqlInt64 :: HDBC.SqlValue -> Maybe Int
readSqlInt64 (HDBC.SqlInt64 x) = Just (fromIntegral x)
readSqlInt64 _                 = Nothing

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

get24HoursWeatherRows :: Sqlite3.Connection -> IO (Maybe [Weather])
get24HoursWeatherRows connection = do
  now <- Time.getCurrentTime
  selectByDateRange connection (minusOneDay now) now

getCurrentWeather :: Location -> (Int, Int) -> IO Weather
getCurrentWeather location weather = do
  let (temperature, humidity) = weather
  created <- Time.getCurrentTime
  pure (Weather {temperature, humidity, created, location})

getAndInsertCurrentWeather :: (Int, Int) -> Location -> Sqlite3.Connection -> IO ()
getAndInsertCurrentWeather tempHumidity location connection = do
  weather <- getCurrentWeather location tempHumidity
  _ <- insert connection weather
  pure ()

logWeather :: (Int, Int) -> IO ()
logWeather tempHumidity = do
  connection <- Sqlite3.connectSqlite3 "weather.db"
  getAndInsertCurrentWeather tempHumidity Inside connection
  HDBC.commit connection

getDayOfWeather :: IO (Maybe [Weather])
getDayOfWeather = do
  connection <- Sqlite3.connectSqlite3 "weather.db"
  get24HoursWeatherRows connection
