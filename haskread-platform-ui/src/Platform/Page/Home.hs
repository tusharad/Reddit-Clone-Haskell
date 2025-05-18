{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.Home (homePage) where

import Data.Maybe
import Data.Text (append)
import qualified Data.Text as T
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Platform.View.ThreadCard
import Web.Hyperbole
import qualified Platform.Common.CSS as CSS

data PageParams = PageParams
  { mbCommunityId :: Maybe Int
  , mbUserId :: Maybe Int
  , mbLimit :: Maybe Int
  , mbOffset :: Maybe Int
  }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

newtype HomeId = HomeId Int
  deriving (Show, Read, ViewId, Generic)

instance HyperView HomeId es where
  data Action HomeId = HandlePrev PageParams | HandleNext PageParams
    deriving (Show, Read, ViewAction, Generic)

  update (HandlePrev p) =
    redirect $
      "/"
        <> pageParamsToUrl
          p
            { mbOffset = Just $ maybe 0 (\x -> if x > 10 then x - 10 else 0) (mbOffset p)
            }
  update (HandleNext p) =
    redirect $
      "/"
        <> pageParamsToUrl
          p
            { mbOffset = Just $ maybe 10 (+ 10) (mbOffset p)
            }

pageParamsToUrl :: PageParams -> Url
pageParamsToUrl PageParams {..} = do
  url $
    "?"
      `append` T.intercalate
        "&"
        ( catMaybes
            [ (\x -> "communityId=" `append` toText x) <$> mbCommunityId
            , (\x -> "userId=" `append` toText x) <$> mbUserId
            , (\x -> "offset=" `append` toText x) <$> mbOffset
            , (\x -> "limit=" `append` toText x) <$> mbLimit
            ]
        )

paginationView :: Int -> PageParams -> View HomeId ()
paginationView threadCount pageParams@PageParams {..} =
  let btnCSS =
        cc
          "px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition transform hover:scale-105"
   in tag "nav" (cc "flex justify-center items-center mt-8 py-2 px-2 space-x-2") $ do
        button
          (HandlePrev pageParams)
          (btnCSS . prevFunc)
          "Previous"
        button
          (HandleNext pageParams)
          (btnCSS . nextFunc)
          "Next"
  where
    prevFunc = maybe disabled (\offset_ -> if offset_ == 0 then disabled else const mempty) mbOffset
    nextFunc = if threadCount < 10 then disabled else mempty

homePage ::
  (Hyperbole :> es, IOE :> es) =>
  Eff
    es
    ( Page
        '[ HomeId
         , SortMenuId
         , HeaderId
         , ThreadId
         , FooterId
         , CommunityId
         , LiveSearchId
         , AttachmentViewId
         ]
    )
homePage = do
  mbTokenAndUser <- getTokenAndUser
  mbLimit <- lookupParam "limit"
  mbOffset <- lookupParam "offset"
  mbCommunityId <- lookupParam "communityId"
  mbUserId <- lookupParam "userId"
  eRes <- liftIO (getAllThreads mbLimit mbOffset mbCommunityId mbUserId)
  case eRes of
    Left err -> pure . el_ $ raw (T.pack err)
    Right res -> do
      eUserThreadVotes <-
        liftIO $
          maybe
            (pure (Left "Token not found"))
            (\tokenAndUser -> getUserThreadVotes (fst tokenAndUser) (getThreadIds res))
            mbTokenAndUser
      pure $ el (cc CSS.pageContainerCSS) $ do
        stylesheet "style.css"
        el (cc CSS.flexColumnContainerCSS) $ do
          hyper (HeaderId 1) (headerView $ HeaderOps (fst <$> mbTokenAndUser) (snd <$> mbTokenAndUser))
          tag "main" (cc CSS.mainContainerCSS) $ do
            el (cc CSS.threadListSectionCSS) $ do
              el (cc CSS.threadListMainCSS) $ do
                tag "p" (cc CSS.sectionTitleHomeCSS) "Threads"
                hyper (SortMenuId 1) sortMenuView
                viewThreadsList
                  (snd <$> mbTokenAndUser)
                  (fst <$> mbTokenAndUser)
                  (hush eUserThreadVotes)
                  (threads res)
                hyper (HomeId 1) (paginationView (threadsCount res) PageParams {..})
                hyper (CommunityId 1) communityListView
          hyper (FooterId 1) footerView
  where
    viewThreadsList mUserInfo mToken_ mUserThreadVotes threads =
      foldr
        ( \(idx, thread) acc -> do
            hyper
              (ThreadId idx)
              ( threadView
                  ThreadCardOps
                    { currUserVotesForThreads = mUserThreadVotes
                    , tokenForThreadCard = mToken_
                    , threadInfo = thread
                    , mbUserInfo = mUserInfo
                    }
              )
            acc
        )
        none
        (zip [0 ..] threads)
