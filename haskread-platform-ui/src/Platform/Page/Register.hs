{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.Register
  ( registerPage
  )
where

import Data.Coerce (coerce)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Effectful
import qualified Platform.Common.CSS as CSS
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Web.Hyperbole

data RegisterForm = RegisterForm
  deriving (Show, Read, ViewId, Generic)

instance (IOE :> es, Hyperbole :> es) => HyperView RegisterForm es where
  data Action RegisterForm = Submit | DoRedirect String | OauthPage
    deriving (Show, Read, ViewAction, Generic)

  update (DoRedirect x) = redirect (url . pack $ "/otp/" <> x)
  update OauthPage = do
    redirectUrl <- liftIO getRedirectUrl
    redirect redirectUrl
  update Submit = do
    uf <- formData @(RegisterFormData Identity)
    let vals = validateForm uf
    if or
      [ isInvalid vals.userName
      , isInvalid vals.email
      , isInvalid vals.pass1
      , isInvalid vals.pass2
      ]
      then pure $ registerFormView Nothing vals
      else do
        eRes <- liftIO $ registerUser (coerce uf.userName) uf.email uf.pass1 uf.pass2
        case eRes of
          Left err -> pure $ registerFormView (Just (T.pack err)) vals
          Right resp -> pure $ registerSuccessFullView (userIDForRUR resp)

registerSuccessFullView :: Int -> View RegisterForm ()
registerSuccessFullView newUserId = do
  el (onLoad (DoRedirect $ show newUserId) 10) $ do
    el_ "Register successful, you will be redirected to OTP page. If not please click here. "
    link (url . pack $ "/otp/" <> show newUserId) mempty "Click here"

data RegisterFormData f = RegisterFormData
  { userName :: Field f Text
  , email :: Field f Text
  , pass1 :: Field f Text
  , pass2 :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

validateForm :: RegisterFormData Identity -> RegisterFormData Validated
validateForm u =
  RegisterFormData
    { userName = validateUsername u.userName
    , email = validateEmail (email u)
    , pass1 = validatePass (pass1 u) u.pass2
    , pass2 = NotInvalid
    }

validateUsername :: Text -> Validated Text
validateUsername u = validate (T.length u < 3) "Username too short"

validateEmail :: Text -> Validated Text
validateEmail e =
  mconcat
    [ validate (T.elem ' ' e) "email must not contain spaces"
    , validate (T.length e < 4) "email must be at least 4 chars"
    ]

validatePass :: Text -> Text -> Validated Text
validatePass p1 p2 =
  mconcat
    [ validate (T.length p1 < 3) "Password must be at least 8 chars"
    , validate (p1 /= p2) "Password and Confirm Password do not matched!"
    ]

registerPage :: Eff es (Page '[RegisterForm, HeaderId, FooterId, LiveSearchId, LoginProfileBtns])
registerPage = do
  pure $ el (cc "min-h-screen bg-white dark:bg-gray-900") $ do
    stylesheet "style.css"
    el (cc "flex flex-col min-h-screen") $ do
      hyper (HeaderId 1) headerView
      tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
        el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
          el (cc "w-full lg px-4") $ do
            el (cc "flex flex-col min-h-screen") $ do
              tag "main" (cc "container mx-auto mt-20 px-6 flex-grow") $ do
                tag "h1" (cc "text-2xl font-bold mb-4 text-center") "Register"
                hyper RegisterForm $ registerFormView Nothing genFields
        hyper (FooterId 1) footerView

registerFormView :: Maybe Text -> RegisterFormData Validated -> View RegisterForm ()
registerFormView mErrorMsg _ = do
  let f = fieldNames @RegisterFormData
  el (cc CSS.cardContainerCSS) $ do
    el (cc CSS.paddedCSS) $ do
      form Submit (cc CSS.formBaseCSS) $ do
        field f.userName id $ do
          input
            Username
            ( cc CSS.inputCSS
                . placeholder "Enter user name"
            )

        field (email f) id $ do
          input
            Email
            ( cc CSS.inputCSS
                . placeholder "Enter email"
            )

        field f.pass1 id $ do
          input
            NewPassword
            ( cc CSS.inputCSS
                . placeholder "password"
            )

        field f.pass2 id $ do
          input
            NewPassword
            ( cc CSS.inputCSS
                . placeholder "confirm password"
            )

        case mErrorMsg of
          Nothing -> pure ()
          Just errMsg ->
            el (cc "text-red-500 text-sm") $
              text errMsg

        submit (cc CSS.submitButtonCSS) "Submit"

      button OauthPage (cc CSS.oauthButtonCSS) $ do
        tag "i" (cc "bx bxl-google text-2xl mr-2") "Continue with Google"
