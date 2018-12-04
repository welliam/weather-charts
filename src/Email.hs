{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Email (sendChart) where

import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Monoid                 ((<>))
import           Data.String                 (fromString)
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.Lazy              as TextL
import qualified Data.Text.Lazy              as Text
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import qualified Network.Mail.Mime           as Mime
import qualified System.Environment          as Environment

data Credentials = Credentials{username :: String, password :: String, to :: String} deriving Show

getCredentails :: IO Credentials
getCredentails = makeCreds
  <$> Environment.getEnv "WEATHER_CHARTS_EMAIL_USERNAME"
  <*> Environment.getEnv "WEATHER_CHARTS_EMAIL_TO"
  <*> Environment.getEnv "WEATHER_CHARTS_EMAIL_PASSWORD"
  where makeCreds u t p = Credentials{username=u, to=t, password=p}

settings = SMTP.defaultSettingsSMTPSSL{SMTP.sslPort=465}

sendMail :: Credentials -> Mime.Mail -> SMTP.SMTPConnection -> IO ()
sendMail Credentials{username, to} mail conn
  = Mime.renderMail' mail >>= \mail -> SMTP.sendMail username [to] (BSL.toStrict mail) conn

inlineAttachment
  :: Text.Text     -- contentType
  -> Text.Text     -- filename
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
    htmlBody =  "<img src=\"cid:" <> cid <>  "\" />"

sendChart :: BS.ByteString -> IO ()
sendChart chart
  = SMTP.doSMTPSSLWithSettings "smtp.gmail.com" settings $ \conn -> do
      credentials <- getCredentails
      let Credentials{username, password, to} = credentials
      authenticated <- SMTP.authenticate SMTP.LOGIN username password conn
      print (fromString (take 50 (BS.unpack chart)))
      if authenticated
        then sendMail credentials (inlineAttachment "img/png" "chart.png" chart credentials) conn
        else print "fuck"
