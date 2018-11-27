module Types where

import qualified Data.Time as Time

data Location = Inside | Outside deriving Show

data Weather
  = Weather {created :: Time.UTCTime,
             temperature :: Int,
             humidity :: Int,
             location :: Location}
  deriving Show
