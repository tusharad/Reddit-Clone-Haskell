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

module Platform.Page.Home (homePage) where

import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.ThreadCard
import Web.Hyperbole
import Platform.View.LiveSearch (LiveSearchId)

newtype HomeId = HomeId Int
  deriving (Show, Read, ViewId)

instance HyperView HomeId es where
  data Action HomeId = HandlePrev Int | HandleNext Int
    deriving (Show, Read, ViewAction)

  update (HandlePrev _) = pure paginationView
  update (HandleNext _) = pure paginationView

paginationView :: View HomeId ()
paginationView = tag "nav" (cc "flex justify-center items-center mt-8 py-2 px-2") $ do
  button
    (HandlePrev currentPage)
    (cc "px-3 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-500 mx-1")
    "Previous"
  button
    (HandleNext currentPage)
    (cc "px-3 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-500 mx-1")
    "Next"

currentPage :: Int
currentPage = 1

homePage ::
  (Hyperbole :> es, IOE :> es) =>
  Eff es (Page '[HomeId, SortMenuId, HeaderId, ThreadId, FooterId, CommunityId, LiveSearchId])
homePage = do
  mJwtToken :: Maybe Text <- session "jwt_token"
  res <- liftIO getAllThreads
  mUserInfo_ <- liftIO $ maybe (pure Nothing) getUserInfo mJwtToken
  mUserThreadVotes <-
    liftIO $
      if isJust mJwtToken
        then getUserThreadVotes (fromJust mJwtToken) (getThreadIds res)
        else pure Nothing
  pure $ col (pad 20) $ do
    style globalCSS
    el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
      hyper (HeaderId 1) (headerView mJwtToken mUserInfo_)
      tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
        el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
          el (cc "w-full lg:w-3/4 px-4") $ do
            tag "p" (cc "text-3xl text-center mb-6 text-gray-800") "Threads"
            hyper (SortMenuId 1) sortMenuView
            viewThreadsList mJwtToken mUserThreadVotes 0 (threads res)
          hyper (CommunityId 1) communityListView
      hyper (FooterId 1) footerView
