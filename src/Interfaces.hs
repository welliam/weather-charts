module Interfaces where

import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Time                              as Time
import qualified Database.HDBC                          as HDBC
import qualified Database.HDBC.Sqlite3                  as Sqlite3
import qualified Graphics.Rendering.Chart               as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as CairoChart
import qualified Network.HaskellNet.Auth                as SMPTAuth
import qualified Network.HaskellNet.SMTP.SSL            as SMTP
import qualified Network.Mail.Mime                      as Mime
import qualified Options.Applicative                    as OptParse
import qualified System.Environment                     as Environment
import           System.IO                              (FilePath)
import qualified System.Random                          as Random


class Monad m => MonadSMTP m where
  authenticate :: SMTP.AuthType
    -> SMPTAuth.UserName
    -> SMPTAuth.Password
    -> SMTP.SMTPConnection
    -> m Bool
  sendMail :: SMPTAuth.UserName
    -> [String]
    -> BS.ByteString
    -> SMTP.SMTPConnection
    -> m ()
  doSMTPSSLWithSettings :: String -> SMTP.Settings -> (SMTP.SMTPConnection -> m a) -> m a
  renderMail :: Mime.Mail -> m BSL.ByteString

class Monad m => MonadEnvironment m where
  getEnv :: String -> m String

class Monad m => MonadTime m where
  getCurrentTimeZone :: m Time.TimeZone
  getCurrentTime :: m Time.UTCTime

class Monad m => MonadRandom m where
  random :: Random.Random a => m a

class Monad m => MonadChart m where
  renderableToFile :: CairoChart.FileOptions -> FilePath -> Chart.Renderable () -> m ()

class Monad m => MonadFileSystem m where
  bsReadFile :: FilePath -> m BS.ByteString
  bsWriteFile :: FilePath -> BS.ByteString -> m ()

class Monad m => MonadArgs m where
  execParser :: OptParse.ParserInfo a -> m a

class Monad m => MonadDB m where
  run :: Sqlite3.Connection -> String -> [HDBC.SqlValue] -> m Integer
  quickQuery' :: Sqlite3.Connection -> String -> [HDBC.SqlValue] -> m [[HDBC.SqlValue]]
  connect :: String -> m Sqlite3.Connection
  commit :: Sqlite3.Connection -> m ()

class Monad m => MonadConsole m where
  consolePrint :: Show a => a -> m ()


instance MonadSMTP IO where
  authenticate = SMTP.authenticate
  sendMail = SMTP.sendMail
  doSMTPSSLWithSettings = SMTP.doSMTPSSLWithSettings
  renderMail = Mime.renderMail'

instance MonadEnvironment IO where
  getEnv = Environment.getEnv

instance MonadTime IO where
  getCurrentTimeZone = Time.getCurrentTimeZone
  getCurrentTime = Time.getCurrentTime

instance MonadFileSystem IO where
  bsReadFile = BS.readFile
  bsWriteFile = BS.writeFile

instance MonadChart IO where
  renderableToFile options path chart = do
    _ <- CairoChart.renderableToFile options path chart
    pure ()

instance MonadRandom IO where
  random = Random.randomIO

instance MonadArgs IO where
  execParser = OptParse.execParser

instance MonadDB IO where
  run = HDBC.run
  quickQuery' = HDBC.quickQuery'
  connect = Sqlite3.connectSqlite3
  commit = HDBC.commit

instance MonadConsole IO where
  consolePrint = print
