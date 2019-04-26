{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Args
import qualified Chart
import qualified Data.ByteString as BS
import           Data.Maybe      (maybe)
import qualified Email
import qualified Interfaces
import qualified Queries
import           Types           (Args (..), humidityArg, temperatureArg)

emailChart
  :: (Interfaces.MonadRandom m,
      Interfaces.MonadSMTP m,
      Interfaces.MonadEnvironment m,
      Interfaces.MonadTime m,
      Interfaces.MonadDB m,
      Interfaces.MonadChart m,
      Interfaces.MonadFileSystem m)
  => m ()
emailChart = withChart Email.sendChart

withChart
  :: (Interfaces.MonadRandom m,
      Interfaces.MonadTime m,
      Interfaces.MonadDB m,
      Interfaces.MonadChart m,
      Interfaces.MonadFileSystem m)
  => (BS.ByteString -> m ())
  -> m ()
withChart handler = Queries.getDayOfWeather >>= maybe (pure ()) ((>>= handler) . Chart.render)

saveChart
  :: (Interfaces.MonadRandom m,
      Interfaces.MonadTime m,
      Interfaces.MonadDB m,
      Interfaces.MonadChart m,
      Interfaces.MonadFileSystem m,
      Interfaces.MonadConsole m)
  => m ()
saveChart = do
  withChart (Interfaces.bsWriteFile "chart.html")
  Interfaces.consolePrint "Wrote to chart.html"

handleInput
  :: (Interfaces.MonadRandom m,
      Interfaces.MonadTime m,
      Interfaces.MonadArgs m,
      Interfaces.MonadEnvironment m,
      Interfaces.MonadDB m,
      Interfaces.MonadSMTP m,
      Interfaces.MonadChart m,
      Interfaces.MonadFileSystem m)
  => m ()
handleInput = do
  args <- Args.parseArgs
  case args of
    Insert {temperatureArg, humidityArg} -> Queries.logWeather (temperatureArg, humidityArg)
    Chart -> emailChart

main :: IO ()
main = handleInput
