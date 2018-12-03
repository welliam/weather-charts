module Main where

import Data.Maybe (maybe)
import qualified Queries
import qualified Chart

main :: IO ()
main = do
  Queries.getDayOfWeather >>= maybe (pure ()) Chart.render
