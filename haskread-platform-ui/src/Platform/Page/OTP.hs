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
import Effectful.Reader.Dynamic
import qualified Platform.Common.CSS as CSS
import Platform.Common.Log
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Text.Read (readMaybe)
import Web.Hyperbole

data OTPView = OTPView
  deriving (Show, Read, ViewId, Generic)

instance (IOE :> es, Hyperbole :> es, Reader AppConfig :> es) => HyperView OTPView es where
  data Action OTPView = Submit Int | DoRedirect
    deriving (Show, Read, ViewAction, Generic)

  update DoRedirect = redirect "/login"
  update (Submit newUserId) = do
    logDebug "Submitting OTP"
    uf <- formData @(OTPForm Identity)
    let vals = validateForm uf
    if or [isInvalid vals.otp]
      then pure $ otpView newUserId Nothing vals
      else do
        let otpInt_ = readMaybe $ T.unpack uf.otp
        case otpInt_ of
          Nothing -> pure $ otpView newUserId (Just "OTP verification failed :(") vals
          Just otpInt -> do
            eRes <- liftIO $ verifyOtp newUserId otpInt
            case eRes of
              Left _ -> pure $ otpView newUserId (Just "OTP verification failed :(") vals
              Right _ -> pure userVerificationSuccessView

userVerificationSuccessView :: View OTPView ()
userVerificationSuccessView = do
  el (onLoad DoRedirect 10) $ do
    el_ "User verification sucessful, you will be redirected to login page. If not please click here. "
    link "/login" mempty "Click here"

newtype OTPForm f = OTPForm
  {otp :: Field f Text}
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

validateForm :: OTPForm Identity -> OTPForm Validated
validateForm u =
  OTPForm {otp = validateOTP (otp u)}

validateOTP :: Text -> Validated Text
validateOTP otp_ =
  mconcat
    [ validate (T.length otp_ /= 4) "OTP should be 4 digit long"
    , validate (isNothing (readMaybe $ T.unpack otp_ :: Maybe Int)) "OTP should be numbers only"
    ]

otpPage ::
  Int ->
  Eff es (Page '[OTPView, HeaderId, FooterId, LiveSearchId, LoginProfileBtns])
otpPage newUserId = do
  pure $ do
    stylesheet "style.css"
    el (cc $ CSS.flexColumnContainerCSS <> " " <> CSS.pageBackgroundCSS) $ do
      hyper (HeaderId 1) headerView
      tag "main" (cc CSS.mainContainerCSS) $ do
        el (cc CSS.authLayoutFlexCSS) $ do
          el (cc CSS.authCardContainerCSS) $ do
            el (cc CSS.flexColumnContainerCSS) $ do
              tag "main" (cc "container mx-auto mt-20 px-6 flex-grow") $ do
                tag "h1" (cc CSS.authSectionTitleCSS) "Enter OTP"
                el (cc $ CSS.cardContainerCSS <> " " <> CSS.paddedCSS) $
                  hyper OTPView $
                    otpView newUserId Nothing genFields
        hyper (FooterId 1) footerView

otpView :: Int -> Maybe Text -> OTPForm Validated -> View OTPView ()
otpView newUserId mErrorMsg _ = do
  let f = fieldNames @OTPForm
  form (Submit newUserId) (gap 10 . pad 10) $ do
    field (otp f) success $ do
      input TextInput (inp . placeholder "Enter OTP e.g 1234")

    case mErrorMsg of
      Nothing -> pure ()
      Just errMsg -> el invalid (text errMsg)

    submit btn "Submit"
  where
    inp = inputS
