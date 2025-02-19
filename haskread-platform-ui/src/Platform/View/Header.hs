{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.View.Header
  ( HeaderId (..)
  , HeaderOps (..)
  , headerView
  , defaultCreateThreadData
  , CreateThreadData (..)
  , headerButtonCSS
  , showUserName
  , showLoginAndSignup
  , defaultHeaderOps 
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View.LiveSearch
import Web.Hyperbole
import Web.View.Types

newtype HeaderId = HeaderId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView HeaderId es where
  data Action HeaderId
    = DoLogout
    | AddThread Text UserProfileResponse
    | SubmitCreateThreadForm Text UserProfileResponse
    | CancelCreateThreadForm
    deriving (Show, Read, ViewAction)

  type Require HeaderId = '[LiveSearchId]
  update DoLogout = do
    clearSession "jwt_token"
    pure $ headerView defaultHeaderOps
  update (AddThread token userInfo) = do
    eCommunityList <- liftIO getCommunityList
    case eCommunityList of
      Left err -> do
        liftIO $ putStrLn $ "Error: " <> err
        pure $ headerView $ HeaderOps (Just token) (Just userInfo)
      Right communityList -> do
        pure $
          createThreadView
            communityList
            token
            userInfo
            Nothing
            genForm
  update (SubmitCreateThreadForm token userInfo) = do
    eCommunityList <- liftIO getCommunityList
    case eCommunityList of
      Left err -> do 
        liftIO $ putStrLn $ "Error:" <> err
        redirect "/"
      Right communityList -> do
        uf <- formData @CreateThreadForm
        let vals = validateForm uf
        if anyInvalid vals
          then
            pure $ createThreadView communityList token userInfo Nothing vals
          else do
            eRes <-
              liftIO $
                addThread
                  CreateThreadData
                    { titleForCreateThread = titleField uf
                    , content = descriptionField uf
                    , mToken = Just token
                    , mUserInfo = Just userInfo
                    , communityId = communityIdField uf
                    }
            case eRes of
              Left err -> liftIO $ putStrLn $ "Couldn't insert thread: " <> err
              Right _ -> pure ()
            redirect "/"

  update CancelCreateThreadForm = do
    redirect "/"

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
  Communities ->
  Text ->
  UserProfileResponse ->
  Maybe Text ->
  CreateThreadForm Validated ->
  View HeaderId ()
createThreadView (Communities communityList) token userInfo mErrorMsg v = do
  let f = formFieldsWith v
  let css = "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Create Thread"
      form @CreateThreadForm (SubmitCreateThreadForm token userInfo) (gap 10) $ do
        field (communityIdField f) (const mempty) $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Select community"
            tag "select" (name "communityIdField" . cc "w-full px-2 py-2 border rounded") $
              do
                forM_ communityList $ \c -> do
                  tag
                    "option"
                    (att "value" (toText $ communityID c))
                    (raw $ communityName c)
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
        CancelCreateThreadForm
        (cc "mt-2 px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
        "Cancel"
  where
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id

showLoginAndSignup :: View c ()
showLoginAndSignup = do
  link
    "/register"
    (cc "px-4 py-2 bg-blue-600 text-white rounded-full font-semibold hover:bg-blue-500 transition")
    "Signup"
  link
    "/login"
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
    "/profile"
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

data HeaderOps = HeaderOps {
    mbToken :: Maybe Text
  , mbUserInfo :: Maybe UserProfileResponse
 } deriving (Eq, Show)

defaultHeaderOps :: HeaderOps
defaultHeaderOps = HeaderOps {
    mbToken = Nothing
  , mbUserInfo = Nothing
 } 

headerView :: HeaderOps -> View HeaderId ()
headerView HeaderOps{..} = do
  stylesheet "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css"
  script "https://cdn.tailwindcss.com"
  tag "header" (cc "navbar-bg shadow-lg w-full z-50") $ do
    el (cc "container mx-auto px-6 py-4 flex justify-between items-center") $ do
      link "/" (cc "text-3xl font-bold text-white") "HaskRead"
      el (cc "flex items-center") $ do
        hyper (LiveSearchId 1) (liveSearch mempty [])
      el (cc "ml-4 flex items-center space-x-2") $ do
        case (mbToken,mbUserInfo) of
          (Just token, Just userInfo@UserProfileResponse{..}) -> do 
            button
                  (AddThread token userInfo)
                  (headerButtonCSS "bg-blue-600")
                  "Add Thread"
            showUserName (userNameForUPR, userIDForUPR)
            button DoLogout (headerButtonCSS "bg-yellow-500") "Logout"
          _ -> showLoginAndSignup
