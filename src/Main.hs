module Main where

import qualified Args
import qualified Chart
import           Data.Maybe (maybe)
import qualified Queries

main :: IO ()
main = do
  -- tempHumidity <- Args.getTemperatureHumidity
  Queries.getDayOfWeather >>= maybe (pure ()) ((>>= print) . Chart.render)
