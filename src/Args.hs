module Args (parseArgs) where

import           Control.Applicative ((<$), (<|>))
import           Data.Monoid         ((<>))
import           Options.Applicative (Parser, argument, auto, execParser, flag',
                                      fullDesc, help, helper, info, long,
                                      metavar)
import qualified Types

integer :: String -> Parser Int
integer flag = flag' True (long flag) *> argument auto (metavar "INTEGER" <> help flag)

insertTempHumidity :: Parser Types.Args
insertTempHumidity = mkInsert
  <$> integer "temperature"
  <*> integer "humidity"
  where mkInsert t h = Types.Insert (t, h)

chart :: Parser Types.Args
chart = Types.Chart <$ flag' True (long "chart")

parseArgs :: IO Types.Args
parseArgs = execParser (info (helper <*> (insertTempHumidity <|> chart)) fullDesc)
