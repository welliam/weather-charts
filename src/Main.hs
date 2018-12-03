{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Args
import qualified Chart
import qualified Data.ByteString as BS
import           Data.Maybe      (maybe)
import qualified Queries
import           Types           (Args (..), humidityArg, temperatureArg)
import qualified Types

main :: IO ()
main = do
  args <- Args.parseArgs
  case args of
    Insert {temperatureArg, humidityArg} -> Queries.logWeather (temperatureArg, humidityArg)
    Chart -> Queries.getDayOfWeather
      >>= maybe (pure ()) ((>>= BS.writeFile "chart.html") . Chart.render)
