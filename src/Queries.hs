{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries(getDayOfWeather, logWeather) where

import qualified Data.Time             as Time
import qualified Database.HDBC         as HDBC
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Interfaces

import           Types                 (Location (..), Weather (..), created,
                                        humidity, location, temperature)


weatherQuery
  :: Interfaces.MonadDB m
  => Sqlite3.Connection
  -> String
  -> [HDBC.SqlValue]
  -> m (Maybe [Weather])
weatherQuery connection query args = sequenceA . map readWeatherRow
  <$> Interfaces.quickQuery' connection query args

selectByDateRange
  :: Interfaces.MonadDB m =>
    Sqlite3.Connection
  -> Time.UTCTime
  -> Time.UTCTime
  -> m (Maybe [Weather])
selectByDateRange connection from to = weatherQuery
  connection
  (unlines ["select created, humidity, temperature, location from weather",
            "where created >= ? and created < ?",
            "order by created"])
  [serializeTime from, serializeTime to]

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
  pure Weather{created = created',
               location = location',
               temperature = temperature',
               humidity = humidity'}
readWeatherRow _ = Nothing

minusOneDay :: Time.UTCTime -> Time.UTCTime
minusOneDay time = Time.addUTCTime (-1 * 60 * 60 * 24) time

get24HoursWeatherRows
  :: (Interfaces.MonadDB m, Interfaces.MonadTime m)
  => Sqlite3.Connection -> m (Maybe [Weather])
get24HoursWeatherRows connection = do
  now <- Interfaces.getCurrentTime
  selectByDateRange connection (minusOneDay now) now

getCurrentWeather :: Interfaces.MonadTime m => Location -> (Int, Int) -> m Weather
getCurrentWeather location weather = do
  let (temperature, humidity) = weather
  created <- Interfaces.getCurrentTime
  pure (Weather {temperature, humidity, created, location})

insert :: Interfaces.MonadDB m => Sqlite3.Connection -> Weather -> m Integer
insert connection (Weather {created, humidity, temperature, location}) = Interfaces.run
  connection
  "insert into weather (created, humidity, temperature, location) values (?, ?, ?, ?)"
  [serializeTime created,
    HDBC.SqlInt64 (fromIntegral humidity),
    HDBC.SqlInt64 (fromIntegral temperature),
    serializeLocation location]

getAndInsertCurrentWeather
  :: (Interfaces.MonadDB m, Interfaces.MonadTime m)
  => (Int, Int)
  -> Location
  -> Sqlite3.Connection
  -> m ()
getAndInsertCurrentWeather tempHumidity location connection = do
  weather <- getCurrentWeather location tempHumidity
  _ <- insert connection weather
  pure ()

logWeather :: (Interfaces.MonadTime m, Interfaces.MonadDB m) => (Int, Int) -> m ()
logWeather tempHumidity = do
  connection <- Interfaces.connect "weather.db"
  getAndInsertCurrentWeather tempHumidity Inside connection
  Interfaces.commit connection

getDayOfWeather :: (Interfaces.MonadTime m, Interfaces.MonadDB m) => m (Maybe [Weather])
getDayOfWeather = do
  connection <- Interfaces.connect "weather.db"
  get24HoursWeatherRows connection
