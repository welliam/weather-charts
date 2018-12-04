{-# LANGUAGE NamedFieldPuns #-}
module Email (send) where

import           Data.ByteString    as BS
import           Data.Text.Encoding as Text
import           Network.Mail.Mime  (Address, Mail)
import           Network.Mail.SMTP  as SMTP
import           System.Environment as Environment

data Credentials = Credentials{username :: String, password :: String, to :: String}

getCredentails :: IO Credentials
getCredentails = makeCreds
  <$> Environment.getEnv "WEATHER_CHARTS_EMAIL_USERNAME"
  <$> Environment.getEnv "WEATHER_CHARTS_EMAIL_TO"
  <*> Environment.getEnv "WEATHER_CHARTS_EMAIL_PASSWORD"
  where makeCreds u t p = Credentials{username=u, to=t, password=p}

buildEmail :: Credentials -> BS.ByteString -> Mail
buildEmail Credentials{username, to} chart = SMTP.simpleMail
  Address{addressName=Nothing, addressEmail=username}
  [Address{addressName=Nothing, addressEmail=to}] -- to
  [] -- cc
  [] -- bcc
  "Weather charts" -- subject
  [SMTP.htmlPart (Text.decodeUtf8 chart)]

sendChart :: BS.ByteString -> IO ()
sendChart chart = do
  credentials <- getCredentails
  let Credentials{username, password, to} = credentials
  buildEmail credentials chart
  sendMailWithLogin "gmail.com" username password (Text.utf)
