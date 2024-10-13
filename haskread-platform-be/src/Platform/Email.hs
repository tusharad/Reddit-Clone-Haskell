{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Platform.Email
  ( sendVerificationEmail
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

newtype Email = Email {email :: Text}
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype From = From {email :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data MailGunReqBody = MailGunReqBody
  { from :: From
  , to :: [Email]
  , subject :: Text
  , html :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

{-
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
        (https "api.mailgun.net" /: "v3" /: "mg.tushar-adhatrao.in" /: "messages")
        (ReqBodyJson mkMailerSendReqBody)
        bsResponse
        (oAuth2Bearer (T.encodeUtf8 apiToken))
    if responseStatusCode bs /= 202
      then
        pure $ Left (BSL.fromStrict $ responseBody bs)
      else pure $ Right ()
  where
    mkMailGunReqBody =
      MailGunReqBody
        { from = From fromEmail0
        , to = [Email userEmail0]
        , subject = "Email verification for HaskRead platform"
        , html = (verifyEmailHTMLContent otp)
        }
-}

sendVerificationEmail :: Text -> Text -> Int32 -> IO (Either BSL.ByteString ())
sendVerificationEmail apiToken toEmail otp = runReq defaultHttpConfig $ do
    let url = https "api.mailgun.net" /: "v3" /: "mg.tushar-adhatrao.in" /: "messages"
    let authHeader = basicAuth "api" $ T.encodeUtf8 apiToken
    resp <- req POST url
        (ReqBodyUrlEnc $ mconcat
            [ "from" =: ("haskread@mg.tushar-adhatrao.in" :: Text)
            , "to" =: toEmail
            , "subject" =: ("Email verification for HaskRead platform" :: Text)
            , "text" =: verifyEmailHTMLContent otp
            ])
        bsResponse  -- Changed to get ByteString response
        authHeader
    let statusCode = responseStatusCode resp
    return $ if statusCode >= 200 && statusCode < 300
             then Right ()
             else Left $ BSL.fromStrict $ responseBody resp

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
