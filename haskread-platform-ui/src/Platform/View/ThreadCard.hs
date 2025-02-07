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
import Web.Hyperbole

data ThreadCardOps = ThreadCardOps
  { tokenForThreadCard :: Maybe Text
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
    | EditThread ThreadCardOps
    | SubmitEditThreadForm Int Text UserProfileResponse
    | CancelEditThreadForm
    deriving (Show, Read, ViewAction)

  update (UpdateUpVote threadCardOps@ThreadCardOps {..}) = do
    let threadId = threadIDForThreadInfo threadInfo
    liftIO $ upvoteThread tokenForThreadCard threadId
    pure $
      threadView
        threadCardOps
          { currUserVotesForThreads = updateCurrUserVotes currUserVotesForThreads threadId True
          , threadInfo = updateVoteCount currUserVotesForThreads True threadInfo
          }
  update (UpdateDownVote threadCardOps@ThreadCardOps {..}) = do
    let threadId = threadIDForThreadInfo threadInfo
    liftIO $ downvoteThread tokenForThreadCard threadId
    pure $
      threadView
        threadCardOps
          { currUserVotesForThreads = updateCurrUserVotes currUserVotesForThreads threadId False
          , threadInfo = updateVoteCount currUserVotesForThreads False threadInfo
          }
  update (DeleteThread ThreadCardOps {..}) = do
    let threadId = threadIDForThreadInfo threadInfo
    case tokenForThreadCard of
      Nothing -> redirect "/"
      Just t -> do
        _ <- liftIO $ deleteThread threadId t
        redirect "/"
  update CancelEditThreadForm = redirect "/"
  update (SubmitEditThreadForm tId token userInfo) = do
    uf <- formData @EditThreadForm
    _ <- liftIO $ editThread
      token
      EditThreadData
        { threadIdForEditThread = tId
        , userIdForEditThread = userIDForUPR userInfo
        , titleForEditThread = pure $ titleField uf
        , descriptionForEditThread = pure $ descriptionField uf
        , communityIdForEditThread = pure $ communityIdField uf
        }
    redirect "/"
  update (EditThread ThreadCardOps {..}) = do
    case tokenForThreadCard of
      Nothing -> redirect "/"
      Just t -> do
        case mbUserInfo of
          Nothing -> redirect "/"
          (Just uInfo) -> do
            pure $
              editThreadView
                (threadIDForThreadInfo threadInfo)
                t
                uInfo
                Nothing
                (genEditThreadForm threadInfo)

genEditThreadForm :: ThreadInfo -> EditThreadForm Maybe
genEditThreadForm ThreadInfo {..} =
  EditThreadForm
    { communityIdField = Just communityIDForThreadInfo
    , titleField = Just title
    , descriptionField = pure $ fromMaybe "" description
    }

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
          , tokenForThreadCard = mToken_
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
          (stringToUrl $ "/view-thread/" <> show threadIDForThreadInfo)
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
            if userIDForUPR userInfo == userIDForThreadInfo
              then do
                button
                  (DeleteThread threadCardOps)
                  (cc "text-sm flex hover:bg-gray-900 text-white bg-gray-700 rounded-md px-1 py-1")
                  "delete"
                button
                  (EditThread threadCardOps)
                  (cc "text-sm flex hover:bg-gray-900 text-white bg-gray-700 rounded-md px-1 py-1")
                  "edit"
              else none
        tag "span" (cc "flex items-center space-x-1") $ do
          tag "i" (cc "bx bx-comment") none
          tag "span" mempty . text $ toText (fromMaybe 0 commentCount)

-- Edit thread code
data EditThreadForm f = EditThreadForm
  { communityIdField :: Field f Int
  , titleField :: Field f Text
  , descriptionField :: Field f Text
  }
  deriving (Generic)

instance Form EditThreadForm Maybe

editThreadView ::
  Int ->
  Text ->
  UserProfileResponse ->
  Maybe Text ->
  EditThreadForm Maybe ->
  View ThreadId ()
editThreadView tId token userProfile mErrorMsg v = do
  let f = genFieldsWith v
  let css = "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Create Thread"

      form @EditThreadForm (SubmitEditThreadForm tId token userProfile) (gap 10) $ do
        field (communityIdField f) (const mempty) $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Select community"
            tag "select" (name "communityIdField" . cc "w-full px-2 py-2 border rounded") $
              do
                tag "option" (att "value" "6") "Haskell"
                tag "option" (att "value" "7") "Functional programming"

        field (titleField f) (const mempty) $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Enter title"
            input
              TextInput
              ( placeholder "Title"
                  . cc "w-full px-3 py-2 border rounded"
                  . maybe id value (titleField v)
              )

        field (descriptionField f) mempty $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Enter Description"
            textarea
              (name "descriptionField" . cc "w-full px-3 py-2 border rounded")
              (descriptionField v)

        case mErrorMsg of
          Nothing -> pure ()
          Just errMsg -> el invalid (text errMsg)

        submit
          (btn . cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500")
          "Submit"

      button
        CancelEditThreadForm
        (cc "mt-2 px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
        "Cancel"
