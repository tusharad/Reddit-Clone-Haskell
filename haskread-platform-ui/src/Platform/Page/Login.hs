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
import qualified Platform.Common.CSS as CSS
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Web.Hyperbole

data FormView = FormView
  deriving (Show, Read, ViewId, Generic)

instance (IOE :> es, Hyperbole :> es) => HyperView FormView es where
  data Action FormView = Submit | DoRedirect | OAuthPage
    deriving (Show, Read, ViewAction, Generic)

  update DoRedirect = redirect "/"
  update OAuthPage = do
    redirectUrl <- liftIO getRedirectUrl
    redirect redirectUrl
  update Submit = do
    uf <- formData @(UserForm Identity)
    let vals = validateForm uf
    if or [isInvalid vals.email, isInvalid vals.pass]
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
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

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

loginPage :: Eff es (Page '[FormView, HeaderId, FooterId, LiveSearchId, LoginProfileBtns])
loginPage = do
  pure $ el (cc CSS.pageContainerCSS) $ do
    stylesheet "style.css"
    el (cc CSS.flexColumnContainerCSS) $ do
      hyper (HeaderId 1) headerView
      tag "main" (cc CSS.mainContainerCSS) $ do
        el (cc CSS.authLayoutFlexCSS) $ do
          el (cc "w-full lg px-4") $ do
            el (cc CSS.flexColumnContainerCSS) $ do
              tag "main" (cc "container mx-auto mt-20 px-6 flex-grow") $ do
                tag "p" (cc CSS.authSectionTitleCSS) "Login"
                hyper FormView $ formView Nothing genFields
        hyper (FooterId 1) footerView

formView :: Maybe Text -> UserForm Validated -> View FormView ()
formView mErrorMsg _ = do
  let f = fieldNames @UserForm
  el (cc CSS.cardContainerCSS) $ do
    el (cc CSS.paddedCSS) $ do
      form Submit (cc CSS.formBaseCSS) $ do
        field (email f) id $ do
          tag "label" (cc CSS.labelCSS) $
            tag "span" (cc CSS.labelTextCSS) "email"
          input
            Email
            ( cc CSS.inputCSS
                . placeholder "email"
            )

        field f.pass id $ do
          tag "label" (cc CSS.labelCSS) $
            tag "span" (cc CSS.labelTextCSS) "Password"
          input
            NewPassword
            ( cc CSS.inputCSS
                . placeholder "password"
            )
        case mErrorMsg of
          Nothing -> pure ()
          Just errMsg ->
            el (cc "text-red-500 text-sm") $
              text errMsg
        submit (btn . cc CSS.submitButtonCSS) "Submit"
      button
        OAuthPage
        (btn . cc CSS.oauthButtonCSS)
        $ tag "i" (cc "bx bxl-google text-2xl mr-2") "Continue with Google"
