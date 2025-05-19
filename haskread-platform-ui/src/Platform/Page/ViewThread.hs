{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.ViewThread (viewThreadPage) where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import qualified Platform.Common.CSS as CSS
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.CommentCard
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Platform.View.ThreadCard
import Web.Hyperbole

-- | ViewThreadId is a unique identifier for the thread view.
newtype ViewThreadId = ViewThreadId Int
  deriving (Show, Read, ViewId, Generic)

instance HyperView ViewThreadId es where
  data Action ViewThreadId = InitViewThread
    deriving (Show, Read, ViewAction, Generic)

  update InitViewThread = redirect "/"

-- | Render a list of nested comments.
showCommentsList ::
  Maybe UserProfileResponse ->
  Int ->
  Maybe FetchVoteCommentsForUserResponse ->
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
        , AttachmentViewId
        , LoginProfileBtns
        ]
    )
    ()
showCommentsList mbUserInfo n mUserCommentVotes mToken nestedComments = do
  el (cc "replies ml-4 border-l-2") $ do
    go n nestedComments
  where
    go _ [] = none
    go num (c : cs) = do
      hyper
        (CommentCardId num)
        ( commentCardView $
            CommentCardOps
              { currUserVotes = extractUserVotes <$> mUserCommentVotes
              , tokenForCommentCard = mToken
              , commentInfo = mainComment c
              , mbUserInfoForCommentCard = mbUserInfo
              }
        )
      unless (null (children c)) $
        showCommentsList mbUserInfo (n + 500) mUserCommentVotes mToken (children c)
      go (num + 1) cs

    extractUserVotes (FetchVoteCommentsForUserResponse lst) =
      map (\FetchVoteComments {..} -> (commentIDForFetchVote, isUpvote)) lst

-- | Flatten the comment IDs from a nested comment structure.
flattenCommentIds :: Either String FetchCommentsResponse -> [Int]
flattenCommentIds = either (const []) (flattenCommentIds_ . comments)

flattenCommentIds_ :: [NestedComment] -> [Int]
flattenCommentIds_ = concatMap go
  where
    go c = commentIDForCommentInfo (mainComment c) : flattenCommentIds_ (children c)

-- | Render the thread page.
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
         , AttachmentViewId
         , LoginProfileBtns
         ]
    )
viewThreadPage threadId = do
  mbTokenAndUser <- getTokenAndUser
  eThreadInfo <- liftIO $ getThreadByThreadId threadId
  eCommentList <- liftIO $ getCommentsByThreadId threadId
  eCommunityList <- liftIO getCommunityList

  case eThreadInfo of
    Left err -> pure $ el_ $ raw (T.pack err)
    Right threadInfo -> do
      userData <- fetchUserData (fst <$> mbTokenAndUser) eCommentList threadInfo
      pure $ renderPage (fst <$> mbTokenAndUser) threadInfo eCommentList userData eCommunityList

-- | Fetch user-related data (profile, thread votes, comment votes).
fetchUserData ::
  (IOE :> es) =>
  Maybe Text ->
  Either String FetchCommentsResponse ->
  ThreadInfo ->
  Eff es (Maybe UserProfileResponse, Maybe [(Int, Bool)], Maybe FetchVoteCommentsForUserResponse)
fetchUserData Nothing _ _ = pure (Nothing, Nothing, Nothing)
fetchUserData (Just token) eCommentList threadInfo = do
  eUserInfo <- liftIO $ getUserInfo token
  eUserThreadVotes <- liftIO $ getUserThreadVotes token [threadIDForThreadInfo threadInfo]
  eUserCommentVotes <- liftIO $ getUserCommentVotes token (flattenCommentIds eCommentList)
  return (hush eUserInfo, hush eUserThreadVotes, hush eUserCommentVotes)

-- | Render the entire page.
renderPage ::
  Maybe Text ->
  ThreadInfo ->
  Either String FetchCommentsResponse ->
  (Maybe UserProfileResponse, Maybe [(Int, Bool)], Maybe FetchVoteCommentsForUserResponse) ->
  Either String Communities ->
  Page
    '[ ViewThreadId
     , HeaderId
     , ThreadId
     , CommunityId
     , FooterId
     , CommentCardId
     , LiveSearchId
     , AttachmentViewId
     , LoginProfileBtns
     ]
renderPage
  mToken
  threadInfo
  eCommentList
  (mUserInfo, mUserThreadVotes, mUserCommentVotes)
  eCommunityList =
    el (cc "min-h-screen bg-white dark:bg-gray-900") $ do
      stylesheet "/style.css"
      script "/myjs.js"
      el (cc CSS.flexColumnContainerCSS) $ do
        hyper (HeaderId 1) headerView
        tag "main" (cc CSS.mainContainerCSS) $ do
          el (cc CSS.threadListSectionCSS) $ do
            el (cc CSS.threadListMainCSS) $ do
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
            renderCommunitySection eCommunityList
        hyper (FooterId 1) footerView

-- | Render the comments section.
renderCommentsSection ::
  Maybe Text ->
  ThreadInfo ->
  Either String FetchCommentsResponse ->
  Maybe UserProfileResponse ->
  Maybe FetchVoteCommentsForUserResponse ->
  View
    ( Root
        [ ViewThreadId
        , HeaderId
        , ThreadId
        , CommunityId
        , FooterId
        , CommentCardId
        , LiveSearchId
        , AttachmentViewId
        , LoginProfileBtns
        ]
    )
    ()
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
    tag "h2" (cc CSS.authSectionTitleCSS) "Comments"
    showCommentsList
      mUserInfo
      0
      mUserCommentVotes
      mToken
      (either (const []) comments commentList)

-- | Render the community section.
renderCommunitySection ::
  Either String Communities ->
  View
    ( Root
        [ ViewThreadId
        , HeaderId
        , ThreadId
        , CommunityId
        , FooterId
        , CommentCardId
        , LiveSearchId
        , AttachmentViewId
        , LoginProfileBtns
        ]
    )
    ()
renderCommunitySection (Left err) = el_ $ raw $ T.pack err
renderCommunitySection (Right _) =
  el_ $ hyper (CommunityId 1) $ communityListView
