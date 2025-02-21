{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.ViewThread (viewThreadPage) where

import Control.Monad (unless)
import Data.Either (fromLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.CommentCard
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Platform.View.ThreadCard
import Web.Hyperbole

newtype ViewThreadId = ViewThreadId Int
  deriving (Show, Read, ViewId)

instance HyperView ViewThreadId es where
  data Action ViewThreadId = InitViewThread
    deriving (Show, Read, ViewAction)

  update InitViewThread = redirect "/"

showCommentsList ::
  Maybe UserProfileResponse ->
  Int ->
  Maybe FetchVoteComemntsForUserResponse ->
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
              { currUserVotes =
                  ( \(FetchVoteComemntsForUserResponse lst) ->
                      Just $
                        map
                          (\FetchVoteComments {..} -> (commentIDForFetchVote, isUpvote))
                          lst
                  )
                    =<< mUserCommentVotes
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

flattenCommentIds :: Either String FetchCommentsResponse -> [Int]
flattenCommentIds (Left _) = []
flattenCommentIds (Right (FetchCommentsResponse {comments})) = flattenCommentIds_ comments

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
viewThreadPage threadId = do
  mToken <- jToken <$> session @AuthData
  eThreadInfo <- liftIO $ getThreadByThreadId threadId
  eCommentList <- liftIO $ getCommentsByThreadId threadId
  eCommunityList <- liftIO getCommunityList

  case eThreadInfo of
    Left err -> pure $ el_ $ raw (T.pack err)
    Right threadInfo -> do
      userData <- fetchUserData mToken eCommentList threadInfo
      pure $ renderPage mToken threadInfo eCommentList userData eCommunityList

fetchUserData ::
  (IOE :> es) =>
  Maybe Text ->
  Either String FetchCommentsResponse ->
  ThreadInfo ->
  Eff es (Maybe UserProfileResponse, Maybe [(Int, Bool)], Maybe FetchVoteComemntsForUserResponse)
fetchUserData Nothing _ _ = pure (Nothing, Nothing, Nothing)
fetchUserData (Just token) eCommentList threadInfo = do
  eUserInfo <- liftIO $ getUserInfo token
  eUserThreadVotes <- liftIO $ getUserThreadVotes token [threadIDForThreadInfo threadInfo]
  eUserCommentVotes <- liftIO $ getUserCommentVotes token (flattenCommentIds eCommentList)
  liftIO $ putStrLn $ fromLeft "" eUserInfo
  liftIO $ putStrLn $ fromLeft "" eUserThreadVotes
  liftIO $ putStrLn $ fromLeft "" eUserCommentVotes
  return (hush eUserInfo, hush eUserThreadVotes, hush eUserCommentVotes)

renderPage ::
  Maybe Text ->
  ThreadInfo ->
  Either String FetchCommentsResponse ->
  (Maybe UserProfileResponse, Maybe [(Int, Bool)], Maybe FetchVoteComemntsForUserResponse) ->
  Either String Communities ->
  Page '[ViewThreadId, HeaderId, ThreadId, CommunityId, FooterId, CommentCardId, LiveSearchId]
renderPage
  mToken
  threadInfo
  eCommentList
  (mUserInfo, mUserThreadVotes, mUserCommentVotes)
  eCommunityList =
    col (pad 20) $ do
      stylesheet "style.css"
      el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
        hyper (HeaderId 1) (headerView $ HeaderOps mToken mUserInfo)
        tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
          el (cc "flex flex-wrap lg:flex-nowrap -mx-4") $ do
            el (cc "w-full lg:w-3/4 px-4") $ do
              hyper
                (ThreadId 1)
                ( threadView
                    ThreadCardOps
                      { threadInfo = threadInfo
                      , currUserVotesForThreads = mUserThreadVotes
                      , tokenForThreadCard = mToken
                      , mbUserInfo = mUserInfo
                      }
                )
              renderCommentsSection mToken threadInfo eCommentList mUserInfo mUserCommentVotes
            case eCommunityList of
              Left err -> el_ $ raw $ T.pack err
              Right communityList ->
                el (cc "w-1/4 pl-4") $ hyper (CommunityId 1) $ communityListView communityList
        hyper (FooterId 1) footerView

renderCommentsSection mToken threadInfo commentList mUserInfo mUserCommentVotes = do
  el (cc "mt-6") $ do
    case mToken of
      Nothing -> hyper (CommentCardId 10000) disabledAddCommentButtonView
      Just token ->
        hyper
          (CommentCardId 10000)
          ( addCommentButtonView
              (AddCommentData "" (threadIDForThreadInfo threadInfo) token Nothing)
          )
    tag "h2" (cc "text-2xl font-bold mb-4 text-gray-900") "Comments"
    showCommentsList
      mUserInfo
      0
      mUserCommentVotes
      mToken
      (either (const []) comments commentList)
