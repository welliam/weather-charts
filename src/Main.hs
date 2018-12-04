{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Args
import qualified Chart
import qualified Data.ByteString as BS
import           Data.Maybe      (maybe)
import qualified Email
import qualified Queries
import           Types           (Args (..), humidityArg, temperatureArg)
import qualified Types

withChart :: (BS.ByteString -> IO ()) -> IO ()
withChart f = Queries.getDayOfWeather >>= maybe (pure ()) ((>>= f) . Chart.render)

emailChart :: IO ()
emailChart = withChart Email.sendChart

saveChart :: IO ()
saveChart = do
  withChart (BS.writeFile "chart.html")
  print "Wrote to chart.html"

main :: IO ()
main = do
  args <- Args.parseArgs
  case args of
    Insert {temperatureArg, humidityArg} -> Queries.logWeather (temperatureArg, humidityArg)
    Chart -> saveChart
