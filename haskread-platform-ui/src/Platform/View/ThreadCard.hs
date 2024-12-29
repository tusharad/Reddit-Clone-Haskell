{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.View.ThreadCard
  ( ThreadCardOps (..)
  , ThreadId (..)
  , threadView
  , showDislikeIcon
  , showLikeIcon
  , updateVoteCount
  , voteChanges
  , updateCurrUserVotes
  , Action (..)
  , update
  , viewThreadsList
  ) where

import Data.Maybe
import Data.Text (Text)
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Web.Hyperbole hiding (input)

data ThreadCardOps = ThreadCardOps
  { token :: Maybe Text
  , currUserVotesForThreads :: Maybe [(Int, Bool)]
  , threadInfo :: ThreadInfo
  , mbUserInfo :: Maybe UserProfileResponse
  }
  deriving (Show, Eq, Read)

newtype ThreadId = ThreadId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView ThreadId es where
  data Action ThreadId
    = UpdateUpVote ThreadCardOps
    | UpdateDownVote ThreadCardOps
    | DeleteThread ThreadCardOps
    deriving (Show, Read, ViewAction)

  update (UpdateUpVote threadCardOps@ThreadCardOps {..}) = do
    let threadId = threadIDForThreadInfo threadInfo
    liftIO $ upvoteThread token threadId
    pure $
      threadView
        threadCardOps
          { currUserVotesForThreads = updateCurrUserVotes currUserVotesForThreads threadId True
          , threadInfo = updateVoteCount currUserVotesForThreads True threadInfo
          }
  update (UpdateDownVote threadCardOps@ThreadCardOps {..}) = do
    let threadId = threadIDForThreadInfo threadInfo
    liftIO $ downvoteThread token threadId
    pure $
      threadView
        threadCardOps
          { currUserVotesForThreads = updateCurrUserVotes currUserVotesForThreads threadId False
          , threadInfo = updateVoteCount currUserVotesForThreads False threadInfo
          }
  update (DeleteThread ThreadCardOps{..}) = do 
    let threadId = threadIDForThreadInfo threadInfo
    case token of
      Nothing -> redirect "/"
      Just t -> do 
        _ <- liftIO $ deleteThread threadId t
        redirect "/"

updateVoteCount :: Maybe [(Int, Bool)] -> Bool -> ThreadInfo -> ThreadInfo
updateVoteCount vals isUpvote t =
  let threadId = threadIDForThreadInfo t
      upvotes = fromMaybe 0 (upvoteCount t)
      downvotes = fromMaybe 0 (downvoteCount t)
      (upChange, downChange) = voteChanges vals isUpvote threadId
   in t {upvoteCount = Just (upvotes + upChange), downvoteCount = Just (downvotes + downChange)}

voteChanges :: Maybe [(Int, Bool)] -> Bool -> Int -> (Int, Int)
voteChanges Nothing True _ = (1, 0)
voteChanges Nothing False _ = (0, 1)
voteChanges (Just vals) isUpvote threadId =
  case lookup threadId vals of
    Just True -> if isUpvote then (-1, 0) else (-1, 1) -- If user has already upvoted, then downvote will cancel out the upvote
    Just False -> if isUpvote then (1, -1) else (0, -1) -- If user has already downvoted, then upvote will cancel out the downvote
    Nothing -> if isUpvote then (1, 0) else (0, 1) -- If user has not voted yet, then new vote will be added

updateCurrUserVotes :: Maybe [(Int, Bool)] -> Int -> Bool -> Maybe [(Int, Bool)]
updateCurrUserVotes Nothing tId newVote = Just [(tId, newVote)]
updateCurrUserVotes (Just vals) tId newVote = Just $ case lookup tId vals of
  Just True -> if newVote then filter (\(k, _) -> k /= tId) vals else modifyMap vals tId newVote
  Just False -> if newVote then modifyMap vals tId newVote else filter (\(k, _) -> k /= tId) vals
  Nothing -> (tId, newVote) : vals

modifyMap :: [(Int, Bool)] -> Int -> Bool -> [(Int, Bool)]
modifyMap [] tId newVal = [(tId, newVal)]
modifyMap ((k, v) : xs) tId newVal
  | k == tId = (tId, newVal) : xs
  | otherwise = (k, v) : modifyMap xs tId newVal

showDislikeIcon :: Maybe [(Int, Bool)] -> Int -> View ThreadId ()
showDislikeIcon Nothing _ = tag "i" (cc "bx bx-dislike") none
showDislikeIcon (Just vals) tId = do
  case lookup tId vals of
    Just False -> tag "i" (cc "bx bxs-dislike") none
    _ -> tag "i" (cc "bx bx-dislike") none

showLikeIcon :: Maybe [(Int, Bool)] -> Int -> View ThreadId ()
showLikeIcon Nothing _ = tag "i" (cc "bx bx-like") none
showLikeIcon (Just vals) tId = do
  case lookup tId vals of
    Just True -> tag "i" (cc "bx bxs-like") none
    _ -> tag "i" (cc "bx bx-like") none

viewThreadsList _ _ _ _ [] = none
viewThreadsList mUserInfo mToken_ mUserThreadVotes n (t : ts) = do
  hyper
    (ThreadId n)
    ( threadView
        ThreadCardOps
          { currUserVotesForThreads = mUserThreadVotes
          , token = mToken_
          , threadInfo = t
          , mbUserInfo = mUserInfo
          }
    )
  viewThreadsList mUserInfo mToken_ mUserThreadVotes (n + 1) ts

threadView :: ThreadCardOps -> View ThreadId ()
threadView threadCardOps@ThreadCardOps {threadInfo = ThreadInfo {..}, ..} = do
  el (cc "card-bg shadow-lg rounded-lg mb-6 overflow-hidden") $ do
    el (cc "flex justify-between items-center p-4 border-b") $ do
      tag "h2" (cc "text-lg font-bold text-gray-800") $ do
        link
          (stringToUrl $ "http://localhost:3000/view-thread/" <> show threadIDForThreadInfo)
          mempty
          (text title)
      el (cc "text-sm text-right text-gray-500") $ do
        tag "p" mempty $ do
          text "Community:"
          tag "span" (cc "font-semibold") (text communityNameForThreadInfo)
        tag "p" mempty $ do
          text "Created by:"
          tag "span" (cc "font-semibold") (text userNameForThreadInfo)
        tag "p" mempty "30/02/1997"
    el (cc "p-4") $ do
      tag "p" mempty (text $ fromMaybe "" description)
    el (cc "flex justify-between items-center p-4 border-t") $ do
      el (cc "flex space-x-2 items-center") $ do
        button
          (UpdateUpVote threadCardOps)
          (cc "flex items-center space-x-1 hover:text-blue-500")
          $ do
            showLikeIcon currUserVotesForThreads threadIDForThreadInfo
            tag "span" mempty . text $ toText (fromMaybe 0 upvoteCount)
        button
          (UpdateDownVote threadCardOps)
          (cc "flex items-center space-x-1 hover:text-blue-500")
          $ do
            showDislikeIcon currUserVotesForThreads threadIDForThreadInfo
            tag "span" mempty . text $ toText (fromMaybe 0 downvoteCount)
        case mbUserInfo of
          Nothing -> none
          Just userInfo -> do
            if userIDForUPR userInfo == userIDForThreadInfo then do
              button
                (DeleteThread threadCardOps)
                (cc "text-sm flex hover:bg-gray-900 text-white bg-gray-700 rounded-md px-1 py-1")
                "delete"
            else none
        tag "span" (cc "flex items-center space-x-1") $ do
          tag "i" (cc "bx bx-comment") none
          tag "span" mempty . text $ toText (fromMaybe 0 commentCount)
