{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  , CreateThreadData (..)
  , headerButtonCSS
  , showUserName
  , showLoginAndSignup
  , defaultHeaderOps
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View.LiveSearch
import Web.Hyperbole
import Web.View.Types

newtype HeaderId = HeaderId Int
  deriving (Show, Read, ViewId)

instance (IOE :> es) => HyperView HeaderId es where
  data Action HeaderId
    = DoLogout
    | AddThread
    deriving (Show, Read, ViewAction)

  type Require HeaderId = '[LiveSearchId]
  update DoLogout = do
    _ <- deleteSession @AuthData
    pure $ headerView defaultHeaderOps
  update AddThread = do
    eCommunityList <- liftIO getCommunityList
    case eCommunityList of
      Left err -> do
        liftIO $ putStrLn $ "Error: " <> err
        redirect "/"
      Right communityList -> do
        pure $
          createThreadView
            communityList

createThreadView ::
  Communities ->
  View HeaderId ()
createThreadView (Communities communityList) = do
  let css = "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el
      ( cc
          "bg-white dark:bg-gray-700 shadow-lg rounded-lg mb-6 overflow-hidden hover:shadow-xl transition-shadow duration-300"
      ) $ do
      el (cc "p-6") $ do
        tag "h2" (cc "text-2xl font-bold mb-4 dark:bg-gray-700 dark:border-gray-600 dark:text-white") $
          text "Create Thread"
        tag "div" (gap 10) $ do
          el (cc "mb-4") $ do
            tag "span" (att "id" "statusMessage" . cc "text-green-500") none
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Select community"
            tag
              "select"
              ( cc "w-full px-2 py-2 border rounded"
                  . att "id" "threadCommunityID"
              )
              $ do
                forM_ communityList $ \c -> do
                  tag
                    "option"
                    (att "value" (toText $ communityID c))
                    (raw $ communityName c)
          el (cc "mb-4") $ do
            tag "label" (cc "block dark:bg-gray-700 dark:border-gray-600 dark:text-white") "Enter title"
            tag
              "input"
              ( att "type" "text"
                  . placeholder "Title"
                  . cc "w-full px-3 py-2 border rounded"
                  . att "id" "threadTitle"
              )
              none

          el (cc "mb-4") $ do
            tag "label" (cc "block dark:bg-gray-700 dark:border-gray-600 dark:text-white") "Enter Description"
            tag "textarea" (att "id" "threadDescription" . cc "w-full px-3 py-2 border rounded") none

          el (cc "mb-4") $ do
            tag "label" (cc "block dark:bg-gray-700 dark:border-gray-600 dark:text-white") "Select File"
            tag
              "input"
              (att "id" "threadAttachment" . cc "w-full px-3 py-2 border rounded" . att "type" "file")
              none

          el (cc "mb-4") $ do
            tag
              "button"
              ( att "onClick" "createThread()"
                  . headerButtonCSS "bg-blue-600 hover:bg-blue-500 transition transform hover:scale-105"
              )
              "Create"
            tag
              "button"
              ( att "onClick" "cancelForm()"
                  . headerButtonCSS "bg-blue-600 hover:bg-blue-500 transition transform hover:scale-105"
              )
              "Cancel"

showLoginAndSignup :: View c ()
showLoginAndSignup = do
  link
    "/register"
    (headerButtonCSS "bg-blue-600 hover:bg-blue-500 transition transform hover:scale-105")
    "Signup"
  link
    "/login"
    (headerButtonCSS "bg-yellow-500 hover:bg-yellow-600 transition transform hover:scale-105")
    "Login"

headerButtonCSS :: Text -> Mod id
headerButtonCSS btnColor = cc $ ClassName ("px-4 py-2 text-white rounded-full font-semibold " <> btnColor)

showUserName :: (Text, Int) -> View c ()
showUserName (userName, _) = do
  link
    "/profile"
    (headerButtonCSS "bg-blue-600 hover:bg-blue-500 transition")
    (text userName)

data HeaderOps = HeaderOps
  { mbToken :: Maybe Text
  , mbUserInfo :: Maybe UserProfileResponse
  }
  deriving (Eq, Show)

defaultHeaderOps :: HeaderOps
defaultHeaderOps =
  HeaderOps
    { mbToken = Nothing
    , mbUserInfo = Nothing
    }

headerView :: HeaderOps -> View HeaderId ()
headerView HeaderOps {..} = do
  stylesheet "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css"
  stylesheet "https://cdn.jsdelivr.net/npm/flyonui/dist/full.min.css"
  script "https://cdn.tailwindcss.com"
  script "https://cdn.jsdelivr.net/npm/flyonui/plugin.min.js"
  script "/myjs.js"
  tag "header" (cc "bg-blue-800 shadow-lg w-full z-50") $ do
    el (cc "container mx-auto px-6 py-4 flex flex-col sm:flex-row justify-between items-center") $ do
      link "/" (cc "text-3xl font-bold text-white mb-4 sm:mb-0") "HaskRead"
      el (cc "flex items-center w-full sm:w-auto") $ do
        hyper (LiveSearchId 1) (liveSearch mempty [])
      el (cc "ml-0 sm:ml-4 mt-4 sm:mt-0 flex flex-wrap justify-center space-x-2") $ do
        case (mbToken, mbUserInfo) of
          (Just _, Just UserProfileResponse {..}) -> do
            button
              AddThread
              (headerButtonCSS "bg-blue-600 hover:bg-blue-500 transition transform hover:scale-105")
              "Add Thread"
            showUserName (userNameForUPR, userIDForUPR)
            button
              DoLogout
              (headerButtonCSS "bg-yellow-500 hover:bg-yellow-600 transition transform hover:scale-105")
              "Logout"
          _ -> showLoginAndSignup
