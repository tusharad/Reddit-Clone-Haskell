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
import Platform.View.LiveSearch (LiveSearchId)
import Web.Hyperbole

data FormView = FormView
  deriving (Show, Read, ViewId)

instance (IOE :> es, Hyperbole :> es) => HyperView FormView es where
  data Action FormView = Submit | DoRedirect | OAuthPage
    deriving (Show, Read, ViewAction)

  update DoRedirect = redirect "/"
  update OAuthPage = do
    redirectUrl <- liftIO getRedirectUrl
    redirect redirectUrl
  update Submit = do
    uf <- formData @UserForm
    let vals = validateForm uf
    if anyInvalid vals
      then pure $ formView Nothing vals
      else do
        eRes <- liftIO $ loginUser (email uf) (pass uf)
        case eRes of
          Left _ -> pure $ formView (Just "Login or password is incorrect") vals
          Right r -> do
            saveSession @AuthData (AuthData $ Just r.jwtToken)
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

loginPage :: Eff es (Page '[FormView, HeaderId, FooterId, LiveSearchId])
loginPage = do
  pure $ el (cc "min-h-screen bg-white dark:bg-gray-900") $ do
    stylesheet "style.css"
    el (cc "flex flex-col min-h-screen") $ do
      hyper (HeaderId 1) (headerView defaultHeaderOps)
      tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
        el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
          el (cc "w-full lg px-4") $ do
            el (cc "flex flex-col min-h-screen") $ do
              tag "main" (cc "container mx-auto mt-20 px-6 flex-grow") $ do
                tag "p" (cc "text-2xl font-bold mb-4 text-center") "Login"
                hyper FormView $ formView Nothing genForm
        hyper (FooterId 1) footerView

formView :: Maybe Text -> UserForm Validated -> View FormView ()
formView mErrorMsg v = do
  let f = formFieldsWith v
  el
    ( cc
        "bg-white dark:bg-gray-800 shadow-lg rounded-lg mb-6 overflow-hidden hover:shadow-xl transition-shadow duration-300"
    )
    $ do
      el (cc "p-6") $ do
        form @UserForm Submit (cc "flex flex-col space-y-4") $ do
          field (email f) valStyle $ do
            tag "label" (cc "flex flex-col space-y-1") $
              tag "span" (cc "text-gray-700 dark:text-gray-300") "email"
            input
              Email
              ( cc "w-full px-3 py-2 border rounded dark:bg-gray-700 dark:border-gray-600 dark:text-white"
                  . placeholder "email"
              )
            fv <- fieldValid
            case fv of
              Invalid t -> el_ (text t)
              Valid -> el_ ""
              _ -> none
          field f.pass valStyle $ do
            tag "label" (cc "flex flex-col space-y-1") $
              tag "span" (cc "text-gray-700 dark:text-gray-300") "Password"
            input
              NewPassword
              ( cc "w-full px-3 py-2 border rounded dark:bg-gray-700 dark:border-gray-600 dark:text-white"
                  . placeholder "password"
              )
            el_ invalidText
          case mErrorMsg of
            Nothing -> pure ()
            Just errMsg -> el invalid (text errMsg)
          submit (btn . cc "rounded transition transform hover:scale-105 text-xl mr-2") "Submit"
        button
          OAuthPage
          (btn . cc "mt-2 w-full rounded transition transform hover:scale-105")
          $ tag "i" (cc "bx bxl-google text-2xl mr-2") "Continue with Google"
  where
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id
