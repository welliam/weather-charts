module Main where

import qualified Args
import qualified Chart
import qualified Data.ByteString as BS
import           Data.Maybe      (maybe)
import qualified Queries

main :: IO ()
main = do
  Args.getTemperatureHumidity >>= Queries.logWeather
  Queries.getDayOfWeather >>= maybe (pure ()) ((>>= BS.writeFile "chart.html") . Chart.render)
