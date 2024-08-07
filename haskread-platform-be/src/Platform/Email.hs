{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Platform.Email
  ( sendVerificationEmail,
  )
where

-- Module for all email related tasks

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import GHC.Int (Int32)
import Network.HTTP.Req

data Email = Email {email :: Text}
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data From = From {email :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data MailerSendReqBody = MailerSendReqBody
  { from :: From,
    to :: [Email],
    subject :: Text,
    html :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

sendVerificationEmail ::
  Text ->
  Int32 ->
  Text ->
  Text ->
  IO (Either BSL.ByteString ())
sendVerificationEmail userEmail0 otp apiToken fromEmail0 =
  runReq defaultHttpConfig $ do
    bs <-
      req
        POST
        (https "api.mailersend.com" /: "v1" /: "email")
        (ReqBodyJson mkMailerSendReqBody)
        bsResponse
        (oAuth2Bearer (T.encodeUtf8 apiToken))
    if (responseStatusCode bs /= 202)
      then
        pure $ Left (BSL.fromStrict $ responseBody bs)
      else pure $ Right ()
  where
    mkMailerSendReqBody =
      MailerSendReqBody
        { from = From fromEmail0,
          to = [Email userEmail0],
          subject = "Email verification for HaskRead platform",
          html = (verifyEmailHTMLContent otp)
        }

verifyEmailHTMLContent :: Int32 -> Text
verifyEmailHTMLContent otp =
  [i|
     <p>
        Greetings! <br/>
        <p> Here is your OTP for verifying your email for the 
            <strong>HaskRead Platform</strong>
            <h1> #{show otp} </h1>
     </p>
  |]
