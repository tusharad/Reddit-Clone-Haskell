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
  ) where

import Data.Coerce (coerce)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Web.Hyperbole
import Platform.View.LiveSearch (LiveSearchId)

data RegisterForm = RegisterForm
  deriving (Show, Read, ViewId)

instance (IOE :> es, Hyperbole :> es) => HyperView RegisterForm es where
  data Action RegisterForm = Submit String | DoRedirect String | OauthPage String
    deriving (Show, Read, ViewAction)

  update (DoRedirect x) = redirect (url . pack $ "/otp/" <> x)
  update (OauthPage env) = do
    let redirectUrl = if env == "local" then "http://localhost:8085/api/v1/user/oauth2/login"
                        else "/api/v1/user/oauth2/login"
    redirect redirectUrl
  update (Submit env) = do
    uf <- formData @RegisterFormData
    let vals = validateForm uf
    if anyInvalid vals
      then pure $ registerFormView env Nothing vals
      else do
        mRes <- liftIO $ registerUser (coerce uf.userName) uf.email uf.pass1 uf.pass2
        case mRes of
          Nothing -> pure $ registerFormView env (Just "Something went wrong!") vals
          Just resp -> pure $ registerSuccessFullView (userIDForRUR resp)

registerSuccessFullView :: Int -> View RegisterForm ()
registerSuccessFullView newUserId = do
  el (onLoad (DoRedirect $ show newUserId) 10) $ do
    el_ "Register successful, you will be redirected to OTP page. If not please click here. "
    link (url . pack $ "/otp/" <> show newUserId) mempty "Click here"

newtype User = User {username :: Text}
  deriving (Generic)
  deriving newtype (FromHttpApiData)

data RegisterFormData f = RegisterFormData
  { userName :: Field f User
  , email :: Field f Text
  , pass1 :: Field f Text
  , pass2 :: Field f Text
  }
  deriving (Generic)

instance Form RegisterFormData Validated

validateForm :: RegisterFormData Identity -> RegisterFormData Validated
validateForm u =
  RegisterFormData
    { userName = validateUsername u.userName
    , email = validateEmail (email u)
    , pass1 = validatePass (pass1 u) u.pass2
    , pass2 = NotInvalid
    }

validateUsername :: User -> Validated User
validateUsername (User u) = validate (T.length u < 3) "Username too short"

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

registerPage :: String -> Eff es (Page '[RegisterForm, HeaderId, FooterId, LiveSearchId])
registerPage env = do
  pure $ do
    style globalCSS
    el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
      hyper (HeaderId 1) (headerView Nothing Nothing)
      tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
        el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
          el (cc "w-full lg px-4") $ do
            el (cc "flex flex-col min-h-screen") $ do
              tag "main" (cc "container mx-auto mt-20 px-6 flex-grow") $ do
                tag "h1" (cc "text-2xl font-bold mb-4 text-center") "Register"
                el (cc "card-bg px-6 py-6 shadow-lg rounded-lg mb-6 overflow-hidden") $ do
                  hyper RegisterForm $ registerFormView env Nothing genForm
        hyper (FooterId 1) footerView

registerFormView :: String -> Maybe Text -> RegisterFormData Validated -> View RegisterForm ()
registerFormView env mErrorMsg v = do
  let f = formFieldsWith v
  form @RegisterFormData (Submit env) (gap 10) $ do
    field f.userName valStyle $ do
      input Username (inp . placeholder "Enter user name")
      invalidText

    field (email f) valStyle $ do
      input Email (inp . placeholder "Enter email")
      invalidText

    field f.pass1 valStyle $ do
      input NewPassword (inp . placeholder "password")
      el_ invalidText

    field f.pass2 valStyle $ do
      input NewPassword (inp . placeholder "confirm password")
      el_ invalidText

    case mErrorMsg of
      Nothing -> pure ()
      Just errMsg -> el invalid (text errMsg)
    submit (btn . cc "rounded") "Submit"
  button
    (OauthPage env)
    (cc "mt-2 w-full bg-red-600 text-white py-2 rounded hover:bg-red-700")
    "Continue with Google"
  where
    inp = inputS
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id
