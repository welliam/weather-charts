{-# LANGUAGE NamedFieldPuns #-}
module Args (parseArgs) where

import           Control.Applicative ((<$), (<|>))
import           Data.Monoid         ((<>))
import           Options.Applicative (Parser, argument, auto, execParser, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar)
import           Types               (Args (..), humidityArg, temperatureArg)

longFlag :: String -> Parser a -> Parser a
longFlag = (*>) . flag' True . long

integer :: String -> Parser Int
integer flag = longFlag flag (argument auto (metavar "INTEGER" <> help flag))

insertTempHumidity :: Parser Args
insertTempHumidity = mkInsert
  <$> integer "temperature"
  <*> integer "humidity"
  -- this looks backwards but is actually correct... i hope
  where mkInsert humidityArg temperatureArg  = Insert {temperatureArg, humidityArg}

chart :: Parser Args
chart = longFlag "chart" (pure Chart)

parseArgs :: IO Args
parseArgs = execParser (info (helper <*> (insertTempHumidity <|> chart)) fullDesc)
