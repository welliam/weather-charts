module Main where

import Data.Maybe (maybe)
import qualified Queries

main :: IO ()
main = Queries.getDayOfWeather >>= maybe (pure ()) print
