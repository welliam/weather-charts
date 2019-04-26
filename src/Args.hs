{-# LANGUAGE NamedFieldPuns #-}
module Args  where

import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import qualified Interfaces
import           Options.Applicative (Parser, auto, flag', fullDesc, help,
                                      helper, info, long, metavar, option)
import           Types               (Args (..), humidityArg, temperatureArg)

integer :: String -> Parser Int
integer flag = option auto (long flag <> metavar "INTEGER" <> help flag)

longFlag :: String -> Parser a -> Parser a
longFlag = (*>) . flag' True . long

insertTempHumidity :: Parser Args
insertTempHumidity = mkInsert
  <$> integer "temperature"
  <*> integer "humidity"
  where mkInsert temperatureArg humidityArg = Insert{temperatureArg, humidityArg}

chart :: Parser Args
chart = longFlag "chart" (pure Chart)

parseArgs :: Interfaces.MonadArgs m => m Args
parseArgs = Interfaces.execParser (info (helper <*> (insertTempHumidity <|> chart)) fullDesc)
