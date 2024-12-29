{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.ViewThread (viewThreadPage) where

import Control.Monad (unless)
import Data.Text (Text)
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.CommentCard
import Platform.View.Header
import Platform.View.ThreadCard
import Web.Hyperbole
import Platform.View.LiveSearch (LiveSearchId)

newtype ViewThreadId = ViewThreadId Int
  deriving (Show, Read, ViewId)

instance HyperView ViewThreadId es where
  data Action ViewThreadId = InitViewThread
    deriving (Show, Read, ViewAction)

  update InitViewThread = redirect "/"

showCommentsList ::
  Maybe UserProfileResponse ->
  Int ->
  Maybe [(Int, Bool)] ->
  Maybe Text ->
  [NestedComment] ->
  View
    ( Root
        [ ViewThreadId
        , HeaderId
        , ThreadId
        , CommunityId
        , FooterId
        , CommentCardId
        , LiveSearchId
        ]
    )
    ()
showCommentsList mbUserInfo n mUserCommentVotes mToken nestedComments = do
  el (cc "ml-4 border-l-2 border-gray-300 pl-4") $ do
    go n nestedComments
  where
    go _ [] = none
    go num (c : cs) = do
      hyper
        (CommentCardId num)
        ( commentCardView $
            CommentCardOps
              { currUserVotes = mUserCommentVotes
              , tokenForCommentCard = mToken
              , commentInfo = mainComment c
              , mbUserInfoForCommentCard = mbUserInfo
              }
        )
      unless
        (null (children c))
        ( showCommentsList
            mbUserInfo
            (n + 500)
            mUserCommentVotes
            mToken
            (children c)
        )
      go (num + 1) cs

flattenCommentIds :: Maybe FetchCommentsResponse -> [Int]
flattenCommentIds Nothing = []
flattenCommentIds (Just (FetchCommentsResponse {comments})) = flattenCommentIds_ comments

flattenCommentIds_ :: [NestedComment] -> [Int]
flattenCommentIds_ = concatMap go
  where
    go c = commentIDForCommentInfo (mainComment c) : flattenCommentIds_ (children c)

viewThreadPage ::
  (Hyperbole :> es, IOE :> es) =>
  Int ->
  Eff
    es
    ( Page
        '[ ViewThreadId
         , HeaderId
         , ThreadId
         , CommunityId
         , FooterId
         , CommentCardId
         , LiveSearchId
         ]
    )
viewThreadPage tId = do
  mToken :: Maybe Text <- session "jwt_token"
  mThreadInfo <- liftIO $ getThreadByThreadId tId
  mCommentList <- liftIO $ getCommentsByThreadId tId
  communityList <- liftIO getCommunityList
  case mThreadInfo of
    Nothing -> pure $ el mempty "Hello"
    Just t -> do
      (mUserInfo, mUserThreadVotes, mUserCommentVotes) <- case mToken of
        Nothing -> pure (Nothing, Nothing, Nothing)
        Just token -> do
          mUserInfo <- liftIO $ getUserInfo token
          mUserThreadVotes <- liftIO $ getUserThreadVotes token [threadIDForThreadInfo t]
          mUserCommentVotes <-
            liftIO $
              getUserCommentVotes
                token
                (flattenCommentIds mCommentList)
          liftIO $ print ("mUserCommentVotes" :: String, mUserCommentVotes)
          return (mUserInfo, mUserThreadVotes, mUserCommentVotes)
      pure $ col (pad 20) $ do
        style globalCSS
        el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
          hyper (HeaderId 1) (headerView mToken mUserInfo)
          tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
            el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
              el (cc "w-full lg:w-3/4 px-4") $ do
                hyper
                  (ThreadId 1)
                  (threadView
                      ThreadCardOps
                        { threadInfo = t
                        , currUserVotesForThreads = mUserThreadVotes
                        , token = mToken
                        , mbUserInfo = mUserInfo
                        }
                  )
                el (cc "mt-6") $ do
                  case mToken of
                    Nothing -> hyper (CommentCardId 10000) disabledAddCommentButtonView
                    Just token -> do
                      let addCommentData =
                            AddCommentData
                              { contentForAddComment = ""
                              , threadId = tId
                              , userToken = token
                              , parentCommentIdForAddComment = Nothing
                              }
                      hyper (CommentCardId 10000) (addCommentButtonView addCommentData)
                  tag "h2" (cc "text-2xl font-bold mb-4 text-gray-900") "Comments"
                  showCommentsList mUserInfo 0 mUserCommentVotes mToken (maybe [] comments mCommentList)
              el (cc "w-1/4 pl-4") $ do
                hyper (CommunityId 1) $ communityListView communityList
          hyper (FooterId 1) footerView
