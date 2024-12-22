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

module Platform.Page.Login (loginPage) where

import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Web.Hyperbole

data FormView = FormView
  deriving (Show, Read, ViewId)

instance (IOE :> es, Hyperbole :> es) => HyperView FormView es where
  data Action FormView = Submit | DoRedirect | OAuthPage
    deriving (Show, Read, ViewAction)

  update DoRedirect = redirect "/"
  update OAuthPage = redirect "http://localhost:8085/api/v1/user/oauth2/login"
  update Submit = do
    uf <- formData @UserForm
    let vals = validateForm uf
    if anyInvalid vals
      then pure $ formView Nothing vals
      else do
        mRes <- liftIO $ loginUser (email uf) (pass uf)
        case mRes of
          Nothing -> pure $ formView (Just "Login or password is incorrect") vals
          Just r -> do
            liftIO $ print ("adding value to session " :: String, jwtToken r)
            setSession "jwt_token" (jwtToken r)
            pure loginSuccessFullView

loginSuccessFullView :: View FormView ()
loginSuccessFullView = do
  el (onLoad DoRedirect 10) $ do
    el_ "Login successful, you will be redirected to home page. If not please click here. "
    link "/" mempty "Click here"

data UserForm f = UserForm
  { email :: Field f Text
  , pass :: Field f Text
  }
  deriving (Generic)
instance Form UserForm Validated

validateForm :: UserForm Identity -> UserForm Validated
validateForm u =
  UserForm
    { email = validateEmail (email u)
    , pass = validatePass (pass u)
    }

validateEmail :: Text -> Validated Text
validateEmail e =
  mconcat
    [ validate (T.elem ' ' e) "email must not contain spaces"
    , validate (T.length e < 4) "email must be at least 4 chars"
    ]

validatePass :: Text -> Validated Text
validatePass p1 =
  validate (T.length p1 < 3) "Password must be at least 8 chars"

loginPage :: Eff es (Page '[FormView, HeaderId, FooterId])
loginPage = do
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
                  hyper FormView $ formView Nothing genForm
        hyper (FooterId 1) footerView

formView :: Maybe Text -> UserForm Validated -> View FormView ()
formView mErrorMsg v = do
  let f = formFieldsWith v
  form @UserForm Submit (gap 10) $ do
    field (email f) valStyle $ do
      label "email"
      input Email (inp . placeholder "email")
      fv <- fieldValid
      case fv of
        Invalid t -> el_ (text t)
        Valid -> el_ ""
        _ -> none
    field f.pass valStyle $ do
      label "Password"
      input NewPassword (inp . placeholder "password")
      el_ invalidText
    case mErrorMsg of
      Nothing -> pure ()
      Just errMsg -> el invalid (text errMsg)
    submit (btn . cc "rounded") "Submit"
  button
    OAuthPage
    (cc "mt-2 w-full bg-red-600 text-white py-2 rounded hover:bg-red-700")
    "Continue with Google"
  where
    inp = inputS
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id
