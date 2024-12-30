{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.View.Header
  ( HeaderId (..)
  , headerView
  , defaultCreateThreadData
  , CreateThreadData (..)
  , headerButtonCSS
  , showUserName
  , showLoginAndSignup
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Web.Hyperbole
import Web.View.Types
import Platform.View.LiveSearch

newtype HeaderId = HeaderId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView HeaderId es where
  data Action HeaderId
    = DoLogout
    | AddThread (Maybe Text) (Maybe UserProfileResponse)
    | SubmitCreateThreadForm (Maybe Text) (Maybe UserProfileResponse)
    | CancelCreateThreadForm (Maybe Text) (Maybe UserProfileResponse)
    deriving (Show, Read, ViewAction)

  type Require HeaderId = '[LiveSearchId]
  update DoLogout = do
    clearSession "jwt_token"
    pure $ headerView Nothing Nothing
  update (AddThread mToken mUserInfo) = do
    case mToken of
      Nothing -> pure $ headerView mToken mUserInfo -- Cannot create thread without token
      Just _ -> pure $ createThreadView mToken mUserInfo Nothing genForm -- pure $ createThreadView createThreadData
  update (SubmitCreateThreadForm mToken mUserInfo) = do
    uf <- formData @CreateThreadForm
    let vals = validateForm uf
    if anyInvalid vals
      then
        pure $ createThreadView mToken mUserInfo Nothing vals
      else do
        mRes <-
          liftIO $
            addThread
              CreateThreadData
                { titleForCreateThread = titleField uf
                , content = descriptionField uf
                , mToken = mToken
                , mUserInfo = mUserInfo
                , communityId = communityIdField uf
                }
        case mRes of
          Nothing -> liftIO $ putStrLn "Couldn't insert thread"
          Just _ -> pure ()
        redirect "/"
  update (CancelCreateThreadForm mToken mUserInfo) = do
    pure $ headerView mToken mUserInfo

data CreateThreadForm f = CreateThreadForm
  { communityIdField :: Field f Int
  , titleField :: Field f Text
  , descriptionField :: Field f Text
  }
  deriving (Generic)

instance Form CreateThreadForm Validated

validateForm :: CreateThreadForm Identity -> CreateThreadForm Validated
validateForm u =
  CreateThreadForm
    { communityIdField = NotInvalid
    , titleField = validateTitle (titleField u)
    , descriptionField = NotInvalid
    }

validateTitle :: Text -> Validated Text
validateTitle e =
  mconcat
    [ validate (T.null e) "Title cannot be empty"
    ]

createThreadView ::
  Maybe Text ->
  Maybe UserProfileResponse ->
  Maybe Text ->
  CreateThreadForm Validated ->
  View HeaderId ()
createThreadView mToken mUserInfo mErrorMsg v = do
  let f = formFieldsWith v
  let css = "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Create Thread"
      form @CreateThreadForm (SubmitCreateThreadForm mToken mUserInfo) (gap 10) $ do
        field (communityIdField f) (const mempty) $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Select community"
            tag "select" (name "communityIdField" . cc "w-full px-2 py-2 border rounded") $
              do
                tag "option" (att "value" "6") "Haskell"
                tag "option" (att "value" "7") "Functional programming"

        field (titleField f) valStyle $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Enter title"
            input TextInput (placeholder "Title" . cc "w-full px-3 py-2 border rounded")
            el_ invalidText

        field (descriptionField f) valStyle $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Enter Description"
            tag "textarea" (name "descriptionField" . cc "w-full px-3 py-2 border rounded") none
            el_ invalidText

        case mErrorMsg of
          Nothing -> pure ()
          Just errMsg -> el invalid (text errMsg)

        submit
          (btn . cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500")
          "Submit"

      button
        (CancelCreateThreadForm mToken mUserInfo)
        (cc "mt-2 px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
        "Cancel"
  where
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id

showLoginAndSignup :: View c ()
showLoginAndSignup = do
  link
    "http://localhost:3000/register"
    (cc "px-4 py-2 bg-blue-600 text-white rounded-full font-semibold hover:bg-blue-500 transition")
    "Signup"
  link
    "http://localhost:3000/login"
    (cc "px-4 py-2 bg-gray-200 text-gray-800 rounded-full hover:bg-gray-300 transition")
    "Login"

headerButtonCSS :: Text -> Mod id
headerButtonCSS btnColor =
  cc
    ( ClassName $
        "px-4 py-2 text-white rounded-full font-semibold hover:bg-blue-500 transition " <> btnColor
    )

showUserName :: (Text, Int) -> View c ()
showUserName (userName, _) = do
  link
    "http://localhost:3000/signup"
    (headerButtonCSS "bg-blue-600")
    (text userName)

defaultCreateThreadData :: Maybe Text -> Maybe UserProfileResponse -> CreateThreadData
defaultCreateThreadData mToken mUserInfo =
  CreateThreadData
    { mToken = mToken
    , mUserInfo = mUserInfo
    , communityId = 6
    , titleForCreateThread = ""
    , content = ""
    }

headerView :: Maybe Text -> Maybe UserProfileResponse -> View HeaderId ()
headerView mToken mUserInfo = do
  stylesheet "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css"
  script "https://cdn.tailwindcss.com"
  tag "header" (cc "navbar-bg shadow-lg w-full z-50") $ do
    el (cc "container mx-auto px-6 py-4 flex justify-between items-center") $ do
      link "http://localhost:3000/" (cc "text-3xl font-bold text-white") "HaskRead"
      el (cc "flex items-center") $ do
          hyper (LiveSearchId 1) (liveSearch mempty [])
      el (cc "ml-4 flex items-center space-x-2") $ do
        case mUserInfo of
          Nothing -> showLoginAndSignup
          Just t -> do
            button
              (AddThread mToken mUserInfo)
              (headerButtonCSS "bg-blue-600")
              "Add Thread"
            showUserName (userNameForUPR t, userIDForUPR t)
            button DoLogout (headerButtonCSS "bg-yellow-500") "Logout"
