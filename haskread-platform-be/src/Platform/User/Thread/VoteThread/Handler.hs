{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Thread.VoteThread.Handler
  ( voteThreadH
  , removeVoteThread
  , fetchUserThreadVotesH
  )
where

import Control.Monad (void)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Platform.User.Thread.DB
import Platform.User.Thread.VoteThread.DB
import Platform.User.Thread.VoteThread.Types
import Servant.Auth.Server
import UnliftIO

voteThreadH ::
  (MonadUnliftIO m) =>
  Bool ->
  AuthResult UserInfo ->
  ThreadID ->
  AppM m VoteThreadResponse
voteThreadH isUpvote (Authenticated UserInfo {..}) threadID = do
  void $ checkIfThreadExists threadID
  mVoteThread <- fetchVoteThread threadID userIDForUserInfo
  case mVoteThread of
    Nothing -> addVoteThread userIDForUserInfo threadID isUpvote
    Just voteThread ->
      if isUpvote == threadVote voteThread
        then removeVoteThread userIDForUserInfo threadID
        else updateVoteThread threadID userIDForUserInfo isUpvote
voteThreadH _ _ _ = throw401Err "Unauthorized!"

removeVoteThread :: (MonadUnliftIO m) => UserID -> ThreadID -> AppM m VoteThreadResponse
removeVoteThread userID threadID = do
  eRes :: Either SomeException () <-
    try $ deleteThreadVoteQ userID threadID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ -> pure $ VoteThreadResponse "Vote removed successfully!"

checkIfThreadExists :: (MonadUnliftIO m) => ThreadID -> AppM m ()
checkIfThreadExists threadID = do
  mThread <- fetchThreadByIDQ threadID
  case mThread of
    Nothing -> throw400Err "Thread does not exist!"
    Just _ -> pure ()

fetchVoteThread :: (MonadUnliftIO m) => ThreadID -> UserID -> AppM m (Maybe ThreadVoteRead)
fetchVoteThread threadID userID = do
  eRes :: Either SomeException (Maybe ThreadVoteRead) <-
    try $ fetchThreadVoteQ userID threadID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right r -> pure r

addVoteThread :: (MonadUnliftIO m) => UserID -> ThreadID -> Bool -> AppM m VoteThreadResponse
addVoteThread userID threadID isUpvote = do
  eRes :: Either SomeException () <-
    try $
      addThreadVoteQ $
        ThreadVote
          { threadVoteUserID = userID
          , threadVoteThreadID = threadID
          , threadVote = isUpvote
          , threadVoteCreatedAt = ()
          , threadVoteUpdatedAt = ()
          } -- userID threadID isUpvote
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ -> pure $ VoteThreadResponse "Vote added successfully!"

updateVoteThread :: (MonadUnliftIO m) => ThreadID -> UserID -> Bool -> AppM m VoteThreadResponse
updateVoteThread threadID userID isUpvote = do
  eRes :: Either SomeException () <-
    try $
      updateThreadVoteQ userID threadID $
        ThreadVote
          { threadVoteUserID = userID
          , threadVoteThreadID = threadID
          , threadVote = isUpvote
          , threadVoteCreatedAt = ()
          , threadVoteUpdatedAt = ()
          }
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ -> pure $ VoteThreadResponse "Vote udpated successfully!"

fetchUserThreadVotesH = undefined
