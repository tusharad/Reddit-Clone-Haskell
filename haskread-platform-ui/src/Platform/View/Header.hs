{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  , searchBarView
  , showLoginAndSignup
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Web.Hyperbole
import Web.Hyperbole.View.Event (onInput)
import qualified Web.View.Element as Element
import Web.View.Types

newtype HeaderId = HeaderId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView HeaderId es where
  data Action HeaderId
    = DoLogout
    | AddThread CreateThreadData
    | UpdateDropDown CreateThreadData (Maybe Int)
    | UpdateTitleInput CreateThreadData Text
    | UpdateContentInput CreateThreadData Text
    | SubmitCreateThreadForm CreateThreadData
    | CancelCreateThreadForm CreateThreadData
    deriving (Show, Read, ViewAction)

  update DoLogout = do
    clearSession "jwt_token"
    pure $ headerView Nothing Nothing
  update (AddThread createThreadData@CreateThreadData {..}) = do
    case mToken of
      Nothing -> pure $ headerView mToken mUserInfo -- Cannot create thread without token
      Just _ -> pure $ createThreadView createThreadData
  update (UpdateDropDown createThreadData newMCommunityId) = do
    pure $ createThreadView createThreadData {mCommunityId = newMCommunityId}
  update (UpdateTitleInput createThreadData newTitle) = do
    pure $ createThreadView createThreadData {titleForCreateThread = newTitle}
  update (UpdateContentInput createThreadData newContent) = do
    pure $ createThreadView createThreadData {content = newContent}
  update (SubmitCreateThreadForm c) = do
    void . liftIO $ addThread c
    redirect "/"
  update (CancelCreateThreadForm CreateThreadData {..}) = do
    pure $ headerView mToken mUserInfo

createThreadView ::
  CreateThreadData -> View HeaderId ()
createThreadView createThreadData@CreateThreadData {..} = do
  let css = "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Create Thread"

      el (cc "mb-4") $ do
        tag "label" (cc "block text-gray-700") "Select community"
        dropdown
          (UpdateDropDown createThreadData)
          (== mCommunityId)
          (cc "w-full px-3 py-2 border rounded")
          $ do
            option Nothing "Haskell"
            option (Just 7) "Functional programming"

      el (cc "mb-4") $ do
        tag "label" (cc "block text-gray-700") "Enter title"
        tag
          "input"
          ( cc "w-full px-3 py-2 border rounded"
              <> onInput (UpdateTitleInput createThreadData) 500
          )
          none

      el (cc "mb-4") $ do
        tag "label" (cc "block text-gray-700") "Enter Description"
        tag
          "textarea"
          ( cc "w-full px-3 py-2 border rounded"
              <> att "rows" "4"
              <> onInput (UpdateContentInput createThreadData) 500
          )
          none

      el (cc "flex justify-end space-x-2") $ do
        button
          (CancelCreateThreadForm createThreadData)
          (cc "px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
          "Cancel"
        button
          (SubmitCreateThreadForm createThreadData)
          (cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500")
          "Submit"

searchBarView :: Text -> View HeaderId ()
searchBarView = undefined

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
    , mCommunityId = Nothing
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
        el (cc "relative") $ do
          Element.input (cc "pl-2 pr-4 py-2 border rounded-full focus:ring-2 focus:ring-blue-600")
          tag
            "i"
            (cc "bx bx-search-alt bg-white rounded-md px-3 py-3 ml-2")
            none
      el (cc "ml-4 flex items-center space-x-2") $ do
        case mUserInfo of
          Nothing -> showLoginAndSignup
          Just t -> do
            button
              (AddThread $ defaultCreateThreadData mToken mUserInfo)
              (headerButtonCSS "bg-blue-600")
              "Add Thread"
            showUserName (userNameForUPR t, userIDForUPR t)
            button DoLogout (headerButtonCSS "bg-yellow-500") "Logout"
