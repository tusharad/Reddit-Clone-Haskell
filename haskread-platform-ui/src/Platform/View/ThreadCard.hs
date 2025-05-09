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
  , AttachmentViewId (..)
  , threadView
  , showDislikeIcon
  , showLikeIcon
  , updateVoteCount
  , voteChanges
  , updateCurrUserVotes
  , Action (..)
  , update
  ) where

import Control.Monad (forM_, void)
import Data.Base64.Types
import Data.ByteString.Lazy.Base64
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import System.FilePath
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

newtype AttachmentViewId = AttachmentViewId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView AttachmentViewId es where
  data Action AttachmentViewId = LoadImage Int
    deriving (Show, Read, ViewAction)

  update (LoadImage threadId) = do
    eRes <- liftIO $ getAttachment threadId
    case eRes of
      Left _ -> pure $ el_ "Failed to load attachment"
      Right docContent -> pure $
        el (cc "p-4") $ do
          tag
            "img"
            ( att
                "src"
                ( TL.toStrict $
                    "data:image/jpeg;base64,"
                      <> extractBase64 (encodeBase64 docContent)
                )
                . att "alt" "error...image supposed to be here"
                . cc "w-full h-auto rounded-md"
            )
            none

instance IOE :> es => HyperView ThreadId es where
  data Action ThreadId
    = UpdateUpVote ThreadCardOps
    | UpdateDownVote ThreadCardOps
    | DeleteThread ThreadCardOps
    | EditThread ThreadCardOps
    | CancelEditThreadForm
    deriving (Show, Read, ViewAction)

  type Require ThreadId = '[AttachmentViewId]

  update (UpdateUpVote threadCardOps@ThreadCardOps {..}) = do
    if isNothing tokenForThreadCard
      then pure $ threadView threadCardOps
      else do
        let threadId = threadIDForThreadInfo threadInfo
        void . liftIO $ upvoteThread tokenForThreadCard threadId
        pure $
          threadView
            threadCardOps
              { currUserVotesForThreads = updateCurrUserVotes currUserVotesForThreads threadId True
              , threadInfo = updateVoteCount currUserVotesForThreads True threadInfo
              }
  update (UpdateDownVote threadCardOps@ThreadCardOps {..}) = do
    if isNothing tokenForThreadCard
      then pure $ threadView threadCardOps
      else do
        let threadId = threadIDForThreadInfo threadInfo
        void . liftIO $ downvoteThread tokenForThreadCard threadId
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
  update (EditThread ThreadCardOps {..}) = do
    eCommunityList <- liftIO getCommunityList
    case eCommunityList of
      Left err -> do
        liftIO $ putStrLn $ "Error: " <> err
        redirect "/"
      Right communityList -> do
        case mbUserInfo of
          Nothing -> redirect "/"
          (Just _) -> do
            pure $
              editThreadView
                threadInfo
                communityList

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
    Just True -> if isUpvote then (-1, 0) else (-1, 1)
    -- If user has already upvoted, then downvote will cancel out the upvote
    Just False -> if isUpvote then (1, -1) else (0, -1)
    -- If user has already downvoted, then upvote will cancel out the downvote
    Nothing -> if isUpvote then (1, 0) else (0, 1)

-- If user has not voted yet, then new vote will be added

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
showDislikeIcon Nothing _ = tag "i" (cc "bx bx-dislike text-gray-600 dark:text-gray-300") none
showDislikeIcon (Just vals) tId = do
  case lookup tId vals of
    Just False -> tag "i" (cc "bx bxs-dislike text-gray-600 dark:text-gray-300") none
    _ -> tag "i" (cc "bx bx-dislike text-gray-600 dark:text-gray-300") none

showLikeIcon :: Maybe [(Int, Bool)] -> Int -> View ThreadId ()
showLikeIcon Nothing _ = tag "i" (cc "bx bx-like text-gray-600 dark:text-gray-300") none
showLikeIcon (Just vals) tId = do
  case lookup tId vals of
    Just True -> tag "i" (cc "bx bxs-like text-gray-600 dark:text-gray-300") none
    _ -> tag "i" (cc "bx bx-like text-gray-600 dark:text-gray-300") none

attachmentView :: Text -> Int -> View AttachmentViewId ()
attachmentView attachmentName threadId
  | isImage = el (cc "mb-4" . onLoad (LoadImage threadId) 500) $ text "Loading attachment..."
  | otherwise = el (cc "mb-4") $ do
      let funcName = mconcat ["downloadAttachment(", T.pack (show threadId), ",'", attachmentName, "')"]
      tag
        "button"
        (cc "text-blue-500" . att "onClick" funcName)
        (text attachmentName)
  where
    isImage =
      takeExtension
        (T.unpack attachmentName)
        `elem` [".png", ".jpeg", ".jpg", ".svg", ".gif"]

threadView :: ThreadCardOps -> View ThreadId ()
threadView threadCardOps@ThreadCardOps {threadInfo = ThreadInfo {..}, ..} = do
  el
    ( cc
        "bg-white dark:bg-gray-800 shadow-lg rounded-lg mb-6 overflow-hidden hover:shadow-xl transition-shadow duration-300"
    ) $ do
    el
      ( cc
          "flex flex-col sm:flex-row justify-between items-start sm:items-center p-4 border-b dark:border-gray-700"
      ) $ do
      tag "h2" (cc "text-lg font-bold text-gray-800 dark:text-gray-200") $ do
        link
          (stringToUrl $ "/view-thread/" <> show threadIDForThreadInfo)
          (cc "truncate block hover:text-blue-600 dark:hover:text-blue-400 transition")
          (text title)
      el (cc "text-sm text-gray-500 dark:text-gray-400 mt-2 sm:mt-0") $ do
        tag "p" mempty $ do
          text "Community:"
        tag "span" (cc "font-semibold") (text communityNameForThreadInfo)
        tag "p" mempty $ do
          text "Created by:"
          tag "span" (cc "font-semibold") (text userNameForThreadInfo)
        tag "p" mempty (text createdAtForThreadInfo)
    el (cc "p-4") $ do
      tag "p" mempty (text $ fromMaybe "" description)
      case attachmentName of
        (Just attName) ->
          hyper
            (AttachmentViewId threadIDForThreadInfo)
            (attachmentView attName threadIDForThreadInfo)
        Nothing -> none
    el (cc "flex justify-between items-center p-4 border-t dark:border-gray-700") $ do
      el (cc "flex space-x-2 items-center") $ do
        button
          (UpdateUpVote threadCardOps)
          ( cc
              "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
          )
          $ do
            showLikeIcon currUserVotesForThreads threadIDForThreadInfo
            tag "span" (cc "text-gray-600 dark:text-gray-300") . text $ toText (fromMaybe 0 upvoteCount)
        button
          (UpdateDownVote threadCardOps)
          ( cc
              "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
          )
          $ do
            showDislikeIcon currUserVotesForThreads threadIDForThreadInfo
            tag "span" (cc "text-gray-600 dark:text-gray-300") . text $ toText (fromMaybe 0 downvoteCount)
        case mbUserInfo of
          Nothing -> none
          Just userInfo -> do
            if userIDForUPR userInfo == userIDForThreadInfo
              then do
                button
                  (DeleteThread threadCardOps)
                  ( cc
                      "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
                  )
                  "delete"
                button
                  (EditThread threadCardOps)
                  ( cc
                      "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
                  )
                  "edit"
              else none
        tag "span" (cc "flex items-center space-x-1 text-gray-600 dark:text-gray-300") $ do
          tag "i" (cc "bx bx-comment") none
          tag "span" mempty . text $ toText (fromMaybe 0 commentCount)

editThreadView :: ThreadInfo -> Communities -> View ThreadId ()
editThreadView ThreadInfo {..} (Communities communityList) = do
  let css = "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
      funcName = "updateThread(" <> show threadIDForThreadInfo <> ")"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Create Thread"
      tag "div" (gap 10) $ do
        el (cc "mb-4") $ do
          tag "span" (att "id" "statusMessage" . cc "text-green-500") none
        el (cc "mb-4") $ do
          tag "label" (cc "block text-gray-700") "Select community"
          tag
            "select"
            ( cc "w-full px-2 py-2 border rounded"
                . att "id" "threadCommunityID"
            )
            $ do
              forM_ communityList $ \c -> do
                tag
                  "option"
                  (att "value" (toText $ communityID c))
                  (raw $ communityName c)
        el (cc "mb-4") $ do
          tag "label" (cc "block text-gray-700") "Enter title"
          tag
            "input"
            ( att "type" "text"
                . placeholder "Title"
                . cc "w-full px-3 py-2 border rounded"
                . att "id" "threadTitle"
                . att "value" title
            )
            none

        el (cc "mb-4") $ do
          tag "label" (cc "block text-gray-700") "Enter Description"
          tag
            "textarea"
            ( att "id" "threadDescription"
                . maybe mempty (att "value") description
                . cc "w-full px-3 py-2 border rounded"
            )
            none

        el (cc "mb-4") $ do
          tag
            "label"
            (cc "block text-gray-700")
            "Cannot update file, if want to update, please delete post"

        el (cc "mb-4") $ do
          tag
            "button"
            ( att "onClick" (T.pack funcName)
                . cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500"
            )
            "Create"
          tag
            "button"
            ( att "onClick" "cancelForm()"
                . cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500"
            )
            "Cancel"
