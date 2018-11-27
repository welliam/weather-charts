module Args (getTemperatureHumidity) where

import Options.Applicative
  (Parser
  , argument
  , str
  , helper
  , help
  , info
  , fullDesc
  , auto
  , metavar
  , execParser
  )
import Data.Monoid ((<>))

sample :: Parser (Int, Int)
sample = (\temperature humidity -> (temperature, humidity))
     <$> argument auto (metavar "INTEGER" <> help "Temperature")
     <*> argument auto (metavar "INTEGER" <> help "Humidity")

getTemperatureHumidity :: IO (Int, Int)
getTemperatureHumidity = execParser (info (helper <*> sample) fullDesc)
