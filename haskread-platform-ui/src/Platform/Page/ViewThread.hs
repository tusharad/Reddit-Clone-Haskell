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
  Maybe [FetchVoteComments] ->
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

    extractUserVotes lst =
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
  eThreadInfo <- liftIO $ getThreadByThreadId threadId
  eCommentList <- liftIO $ getCommentsByThreadId threadId
  eCommunityList <- liftIO getCommunityList
  uc <- genUserContext [threadId] (flattenCommentIds eCommentList)
  case eThreadInfo of
    Left err -> pure $ el_ $ raw (T.pack err)
    Right threadInfo -> pure $ renderPage threadInfo eCommentList uc eCommunityList

-- | Render the entire page.
renderPage ::
  ThreadInfo ->
  Either String FetchCommentsResponse ->
  Maybe UserContext ->
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
  threadInfo
  eCommentList
  mbUserContext
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
                (threadView threadInfo mbUserContext)
              renderCommentsSection threadInfo eCommentList mbUserContext
            renderCommunitySection eCommunityList
        hyper (FooterId 1) footerView

-- | Render the comments section.
renderCommentsSection ::
  ThreadInfo ->
  Either String FetchCommentsResponse ->
  Maybe UserContext ->
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
renderCommentsSection threadInfo commentList mbUserContext = do
  el (cc "mt-6") $ do
    case mbUserContext of
      Nothing -> hyper (CommentCardId 10000) disabledAddCommentButtonView
      Just UserContext {..} ->
        hyper
          (CommentCardId 10000)
          ( addCommentButtonView
              (AddCommentData "" (threadIDForThreadInfo threadInfo) ucToken Nothing)
          )
    tag "h2" (cc CSS.authSectionTitleCSS) "Comments"
    showCommentsList
      (ucUserProfile <$> mbUserContext)
      0
      (ucUserCommentVotes <$> mbUserContext)
      (ucToken <$> mbUserContext)
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
