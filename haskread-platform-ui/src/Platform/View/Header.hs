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
  , LoginProfileBtns (..)
  , headerView
  , CreateThreadData (..)
  , headerButtonCSS
  , showUserName
  , showLoginAndSignup
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import Effectful
import Platform.Common.CSS
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View.LiveSearch
import Web.Hyperbole
import Web.View.Types

newtype LoginProfileBtns = LoginProfileBtns Int
  deriving (Show, Read, ViewId, Generic)

instance (IOE :> es) => HyperView LoginProfileBtns es where
  data Action LoginProfileBtns = Load | DoLogout | AddThread
    deriving (Show, Read, ViewAction, Generic)
  update Load = do
    mbUserInfo <- getTokenAndUser
    case mbUserInfo of
      Nothing -> pure showLoginAndSignup
      Just (_, userProfileInfo) -> pure $ profileBtns userProfileInfo
  update DoLogout = do
    _ <- deleteSession @AuthData
    redirect "/"
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

newtype HeaderId = HeaderId Int
  deriving (Show, Read, ViewId, Generic)

instance HyperView HeaderId es where
  data Action HeaderId = Init
    deriving (Show, Generic, ViewAction, Read)
  update Init = redirect "/"
  type Require HeaderId = '[LiveSearchId, LoginProfileBtns]

createThreadView ::
  Communities ->
  View LoginProfileBtns ()
createThreadView (Communities communityList) = do
  el (cc centeredCSS) $ do
    el (cc cardContainerCSS) $ do
      el (cc paddedCSS) $ do
        tag "h2" (cc sectionTitleCSS) $ text "Create Thread"
        tag "div" (gap 10) $ do
          el (cc formGroupCSS) $ tag "span" (att "id" "statusMessage" . cc "text-green-500") none
          el (cc formGroupCSS) $ do
            tag "label" (cc labelCSS) "Select community"
            tag "select" (cc selectCSS . att "id" "threadCommunityID") $ do
              forM_ communityList $ \c ->
                tag "option" (att "value" (toText $ communityID c)) (raw $ communityName c)
          el (cc formGroupCSS) $ do
            tag "label" (cc labelCSS) "Enter title"
            tag
              "input"
              (att "type" "text" . placeholder "Title" . cc inputCSS . att "id" "threadTitle")
              none
          el (cc formGroupCSS) $ do
            tag "label" (cc labelCSS) "Enter Description"
            tag "textarea" (att "id" "threadDescription" . cc inputCSS) none
          el (cc formGroupCSS) $ do
            tag "label" (cc labelCSS) "Select File"
            tag "input" (att "id" "threadAttachment" . cc inputCSS . att "type" "file") none
          el (cc formGroupCSS) $ do
            tag "button" (att "onClick" "createThread()" . headerButtonCSS buttonCSS) "Create"
            tag "button" (att "onClick" "cancelForm()" . headerButtonCSS buttonCSS) "Cancel"

showLoginAndSignup :: View c ()
showLoginAndSignup =
  el (cc "row") $ do
    link "/register" (headerButtonCSS primaryButtonCSS) "Signup"
    link "/login" (headerButtonCSS secondaryButtonCSS) "Login"

headerButtonCSS :: ClassName -> Mod id
headerButtonCSS btnColor = cc $ "px-4 py-2 text-white rounded-full font-semibold " <> btnColor

showUserName :: (Text, Int) -> View c ()
showUserName (userName, _) = do
  link "/profile" (headerButtonCSS "bg-blue-600 hover:bg-blue-500 transition") (text userName)

loginOrProfileButtons :: View LoginProfileBtns ()
loginOrProfileButtons = do
  el (onLoad Load 500) $ showLoginAndSignup

profileBtns :: UserProfileResponse -> View LoginProfileBtns ()
profileBtns UserProfileResponse {..} =
  el (cc "row") $ do
    button AddThread (headerButtonCSS blueBgButtonCSS) "Add Thread"
    showUserName (userNameForUPR, userIDForUPR)
    button DoLogout (headerButtonCSS yellowBgButtonCSS) "Logout"

headerView :: View HeaderId ()
headerView = do
  stylesheet "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css"
  stylesheet "https://cdn.jsdelivr.net/npm/flyonui/dist/full.min.css"
  script "https://cdn.tailwindcss.com"
  script "https://cdn.jsdelivr.net/npm/flyonui/plugin.min.js"
  script "/myjs.js"

  tag "header" (cc headerCSS) $ do
    el (cc containerFlexCSS) $ do
      link "/" (cc siteTitleCSS) "HaskRead"
      el (cc navContainerCSS) $ do
        hyper (LiveSearchId 1) (liveSearch mempty [])
      el (cc searchContainerCSS) $ (hyper (LoginProfileBtns 1) loginOrProfileButtons)
