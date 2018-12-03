module Main where

import qualified Args
import qualified Chart
import qualified Data.ByteString as BS
import           Data.Maybe      (maybe)
import qualified Queries
import qualified Types

main :: IO ()
main = do
  args <- Args.parseArgs
  case args of
    Types.Insert weatherInfo -> Queries.logWeather weatherInfo
    Types.Chart -> Queries.getDayOfWeather
      >>= maybe (pure ()) ((>>= BS.writeFile "chart.html") . Chart.render)
