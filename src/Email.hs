{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Email (sendChart) where

import           Control.Monad               (when)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Monoid                 ((<>))
import           Data.String                 (fromString)
import qualified Data.Text.Lazy              as TextL
import qualified Interfaces
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import qualified Network.Mail.Mime           as Mime

data Credentials = Credentials
  {username :: String,
   password :: String,
   to       :: String,
   host     :: String}
  deriving Show

settings :: SMTP.Settings
settings = SMTP.defaultSettingsSMTPSSL{SMTP.sslPort=465}

getCredentails :: Interfaces.MonadEnvironment m => m Credentials
getCredentails = makeCreds
  <$> Interfaces.getEnv "WEATHER_CHARTS_EMAIL_USERNAME"
  <*> Interfaces.getEnv "WEATHER_CHARTS_EMAIL_TO"
  <*> Interfaces.getEnv "WEATHER_CHARTS_EMAIL_PASSWORD"
  <*> Interfaces.getEnv "WEATHER_CHARTS_EMAIL_HOST"
  where makeCreds username to password host = Credentials{username, to, password, host}

sendMail :: Interfaces.MonadSMTP m => Credentials -> Mime.Mail -> SMTP.SMTPConnection -> m ()
sendMail Credentials{username, to} mail conn = do
  renderedMail <- Interfaces.renderMail mail
  Interfaces.sendMail username [to] (BSL.toStrict renderedMail) conn

inlineAttachment :: TextL.Text -> TextL.Text -> BS.ByteString -> Credentials -> Mime.Mail
inlineAttachment contentType filename bs Credentials{username, to} =
  Mime.addAttachmentBSCid
    (TextL.toStrict contentType)
    (TextL.toStrict filename)
    (BSL.fromStrict bs)
    (TextL.toStrict filename)
    $ mail
  where
    mail = Mime.simpleMailInMemory (fromString to) (fromString username) "chart" "" htmlBody []
    htmlBody = "<img src=\"cid:" <> filename <> "\" />"

sendChartWithCredentials :: Interfaces.MonadSMTP m => Credentials -> BS.ByteString -> m ()
sendChartWithCredentials credentials chart = do
  let Credentials{username, password, host} = credentials
  Interfaces.doSMTPSSLWithSettings host settings $ \conn -> do
    authenticated <- Interfaces.authenticate SMTP.LOGIN username password conn
    when authenticated $
      sendMail credentials (inlineAttachment "img/png" "chart.png" chart credentials) conn

sendChart :: (Interfaces.MonadSMTP m, Interfaces.MonadEnvironment m) => BS.ByteString -> m ()
sendChart chart = getCredentails >>= flip sendChartWithCredentials chart
