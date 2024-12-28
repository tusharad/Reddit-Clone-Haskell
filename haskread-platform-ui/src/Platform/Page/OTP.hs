{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.OTP (otpPage) where

import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Platform.Common.Request
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Text.Read (readMaybe)
import Web.Hyperbole
import Platform.View.LiveSearch (LiveSearchId)

data OTPView = OTPView
  deriving (Show, Read, ViewId)

instance (IOE :> es, Hyperbole :> es) => HyperView OTPView es where
  data Action OTPView = Submit Int | DoRedirect
    deriving (Show, Read, ViewAction)

  update DoRedirect = redirect "/login"
  update (Submit newUserId) = do
    uf <- formData @OTPForm
    let vals = validateForm uf
    if anyInvalid vals
      then pure $ otpView newUserId Nothing vals
      else do
        let otpInt_ = readMaybe $ T.unpack uf.otp
        case otpInt_ of
          Nothing -> pure $ otpView newUserId (Just "OTP verification failed :(") vals
          Just otpInt -> do
            mRes <- liftIO $ verifyOtp newUserId otpInt
            case mRes of
              Nothing -> pure $ otpView newUserId (Just "OTP verification failed :(") vals
              Just _ -> pure userVerificationSuccessView

userVerificationSuccessView :: View OTPView ()
userVerificationSuccessView = do
  el (onLoad DoRedirect 10) $ do
    el_ "User verification sucessful, you will be redirected to login page. If not please click here. "
    link "/login" mempty "Click here"

newtype OTPForm f = OTPForm
  {otp :: Field f Text}
  deriving (Generic)

instance Form OTPForm Validated

validateForm :: OTPForm Identity -> OTPForm Validated
validateForm u =
  OTPForm {otp = validateOTP (otp u)}

validateOTP :: Text -> Validated Text
validateOTP otp_ =
  mconcat
    [ validate (T.length otp_ /= 4) "OTP should be 4 digit long"
    , validate (isNothing (readMaybe $ T.unpack otp_ :: Maybe Int)) "OTP should be numbers only"
    ]

otpPage :: Int -> Eff es (Page '[OTPView, HeaderId, FooterId, LiveSearchId])
otpPage newUserId = do
  pure $ do
    style globalCSS
    el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
      hyper (HeaderId 1) (headerView Nothing Nothing)
      tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
        el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
          el (cc "w-full lg px-4") $ do
            el (cc "flex flex-col min-h-screen") $ do
              tag "main" (cc "container mx-auto mt-20 px-6 flex-grow") $ do
                tag "h1" (cc "text-2xl font-bold mb-4 text-center") "Login"
                el (cc "card-bg px-6 py-6 shadow-lg rounded-lg mb-6 overflow-hidden") $ do
                  hyper OTPView $ otpView newUserId Nothing genForm
        hyper (FooterId 1) footerView

otpView :: Int -> Maybe Text -> OTPForm Validated -> View OTPView ()
otpView newUserId mErrorMsg v = do
  let f = formFieldsWith v
  form @OTPForm (Submit newUserId) (gap 10 . pad 10) $ do
    field (otp f) valStyle $ do
      input TextInput (inp . placeholder "Enter OTP e.g 1234")
      invalidText

    case mErrorMsg of
      Nothing -> pure ()
      Just errMsg -> el invalid (text errMsg)

    submit btn "Submit"
  where
    inp = inputS
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id
