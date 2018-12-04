{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Email (sendChart) where

import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Monoid                 ((<>))
import           Data.String                 (fromString)
import qualified Data.Text.Lazy              as TextL
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import qualified Network.Mail.Mime           as Mime
import qualified System.Environment          as Environment

data Credentials = Credentials
  {username :: String,
   password :: String,
   to       :: String,
   host     :: String}
  deriving Show

settings :: SMTP.Settings
settings = SMTP.defaultSettingsSMTPSSL{SMTP.sslPort=465}

getCredentails :: IO Credentials
getCredentails = makeCreds
  <$> Environment.getEnv "WEATHER_CHARTS_EMAIL_USERNAME"
  <*> Environment.getEnv "WEATHER_CHARTS_EMAIL_TO"
  <*> Environment.getEnv "WEATHER_CHARTS_EMAIL_PASSWORD"
  <*> Environment.getEnv "WEATHER_CHARTS_EMAIL_HOST"
  where makeCreds u t p h = Credentials{username=u, to=t, password=p, host=h}

sendMail :: Credentials -> Mime.Mail -> SMTP.SMTPConnection -> IO ()
sendMail Credentials{username, to} mail conn = do
  renderedMail <- Mime.renderMail' mail
  SMTP.sendMail username [to] (BSL.toStrict renderedMail) conn

inlineAttachment
  :: TextL.Text    -- contentType
  -> TextL.Text    -- filename
  -> BS.ByteString -- attachment as bytestring
  -> Credentials
  -> Mime.Mail
inlineAttachment contentType filename bs Credentials{username, to} =
  Mime.addAttachmentBSCid
    (TextL.toStrict contentType)
    (TextL.toStrict cid)
    (BSL.fromStrict bs)
    (TextL.toStrict filename)
    $ mail
  where
    mail = Mime.simpleMailInMemory (fromString to) (fromString username) "chart" "" htmlBody []
    cid = filename
    htmlBody = "<img src=\"cid:" <> cid <> "\" />"

sendChartWithCredentials :: Credentials -> BS.ByteString -> IO ()
sendChartWithCredentials credentials chart = do
  let Credentials{username, host, password} = credentials
  SMTP.doSMTPSSLWithSettings host settings $ \conn -> do
    authenticated <- SMTP.authenticate SMTP.LOGIN username password conn
    if authenticated
      then sendMail credentials (inlineAttachment "img/png" "chart.png" chart credentials) conn
      else putStrLn "Authentication failed!"

sendChart :: BS.ByteString -> IO ()
sendChart chart = getCredentails >>= flip sendChartWithCredentials chart
