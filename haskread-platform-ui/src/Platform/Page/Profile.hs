{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.Profile (profilePage) where

import Data.Text (Text, append)
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.ThreadCard
import Web.Hyperbole

newtype ProfileId = ProfileId Int
  deriving (Show, Read, ViewId)

instance HyperView ProfileId es where
  data Action ProfileId = GoToHome
    deriving (Show, Read, ViewAction)

  update GoToHome = redirect "/"

profilePage ::
  (Hyperbole :> es, IOE :> es) =>
  Eff es (Page '[ProfileId, HeaderId, ThreadId, FooterId])
profilePage = do
  mJwtToken :: Maybe Text <- session "jwt_token"
  case mJwtToken of
    Nothing -> redirect "/"
    Just token_ -> do
      mUserInfo_ <- liftIO $ getUserInfo token_
      pure $ col (pad 20) $ do
        style globalCSS
        hyper (HeaderId 1) (headerView mJwtToken mUserInfo_)
        tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
          el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
            el (cc "mb-6") $ do
              tag "h1" (cc "text-3xl font-bold text-center mb-4") "Profile"
              el (cc "bg-white shadow-lg rounded-lg p-6") $ do
                tag "p" (cc "text-lg text-gray-800") "Welcome to your profile page!"
                tag "p" (cc "text-bold") (text $ "Username:" `append` maybe "" userNameForUPR mUserInfo_)
          hyper (FooterId 1) footerView
