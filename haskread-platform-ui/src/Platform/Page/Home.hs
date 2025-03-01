{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Web.Hyperbole.Data.QueryData (Param (Param))

data PageParams = PageParams
  { mbCommunityId :: Maybe Int
  , mbUserId :: Maybe Int
  , mbLimit :: Maybe Int
  , mbOffset :: Maybe Int
  }
  deriving (Eq, Show, Read)

newtype HomeId = HomeId Int
  deriving (Show, Read, ViewId)

instance HyperView HomeId es where
  data Action HomeId = HandlePrev PageParams | HandleNext PageParams
    deriving (Show, Read, ViewAction)

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
  tag
    "nav"
    (cc "flex justify-center items-center mt-8 py-2 px-2")
    $ do
      button
        (HandlePrev pageParams)
        ( cc
            "px-3 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-500 mx-1"
            . maybe 
                disabled (\offset_ -> if offset_ == 0 then disabled else const mempty) 
                mbOffset
        )
        "Previous"
      button
        (HandleNext pageParams)
        ( cc "px-3 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-500 mx-1"
            . if threadCount < 10 then disabled else mempty
        )
        "Next"

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
  mbLimit <- lookupParam $ Param "limit"
  mbOffset <- lookupParam $ Param "offset"
  mbCommunityId <- lookupParam $ Param "communityId"
  mbUserId <- lookupParam $ Param "userId"
  eRes <- liftIO (getAllThreads mbLimit mbOffset mbCommunityId mbUserId)
  eCommunityList <- liftIO getCommunityList
  case eRes of
    Left err -> pure . el_ $ raw (T.pack err)
    Right res -> do
      eUserThreadVotes <-
        liftIO $
          maybe
            (pure (Left "Token not found"))
            (\tokenAndUser -> getUserThreadVotes (fst tokenAndUser) (getThreadIds res))
            mbTokenAndUser
      pure $ col (pad 20) $ do
        stylesheet "style.css"
        el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
          hyper (HeaderId 1) (headerView $ HeaderOps (fst <$> mbTokenAndUser) (snd <$> mbTokenAndUser))
          tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
            el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
              el (cc "w-full lg:w-3/4 px-4") $ do
                tag "p" (cc "text-3xl text-center mb-6 text-gray-800") "Threads"
                hyper (SortMenuId 1) sortMenuView
                viewThreadsList
                  (snd <$> mbTokenAndUser)
                  (fst <$> mbTokenAndUser)
                  (hush eUserThreadVotes)
                  0
                  (threads res)
                hyper (HomeId 1) (paginationView (threadsCount res) PageParams {..})
              either
                (el_ . raw . T.pack)
                (hyper (CommunityId 1) . communityListView)
                eCommunityList
          hyper (FooterId 1) footerView
