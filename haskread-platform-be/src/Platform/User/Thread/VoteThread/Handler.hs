{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Thread.VoteThread.Handler
  ( voteThreadH
  , removeVoteThread
  , fetchVoteThreadsForUserH
  )
where

import Control.Monad (void)
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
  runQuery $ deleteThreadVoteQ userID threadID
  pure $ VoteThreadResponse "Vote removed successfully!"

checkIfThreadExists :: (MonadUnliftIO m) => ThreadID -> AppM m ()
checkIfThreadExists threadID = do
  mThread <- fetchThreadByIDQ threadID
  case mThread of
    Nothing -> throw400Err "Thread does not exist!"
    Just _ -> pure ()

fetchVoteThread :: (MonadUnliftIO m) => ThreadID -> UserID -> AppM m (Maybe ThreadVoteRead)
fetchVoteThread threadID = runQuery . flip fetchThreadVoteQ threadID

addVoteThread :: (MonadUnliftIO m) => UserID -> ThreadID -> Bool -> AppM m VoteThreadResponse
addVoteThread userID threadID isUpvote = do
  runQuery $
    addThreadVoteQ $
      ThreadVote
        { threadVoteUserID = userID
        , threadVoteThreadID = threadID
        , threadVote = isUpvote
        , threadVoteCreatedAt = ()
        , threadVoteUpdatedAt = ()
        } -- userID threadID isUpvote
  pure $ VoteThreadResponse "Vote added successfully!"

updateVoteThread :: (MonadUnliftIO m) => ThreadID -> UserID -> Bool -> AppM m VoteThreadResponse
updateVoteThread threadID userID isUpvote = do
  runQuery $
    updateThreadVoteQ userID threadID $
      ThreadVote
        { threadVoteUserID = userID
        , threadVoteThreadID = threadID
        , threadVote = isUpvote
        , threadVoteCreatedAt = ()
        , threadVoteUpdatedAt = ()
        }
  pure $ VoteThreadResponse "Vote udpated successfully!"

fetchVoteThreadsForUserH ::
  MonadUnliftIO m =>
  AuthResult UserInfo ->
  FetchVoteThreadsForUserReq ->
  AppM m FetchVoteThreadsForUserResponse
fetchVoteThreadsForUserH
  (Authenticated UserInfo {..})
  (FetchVoteThreadsForUserReq threadList) = do
    -- ThreadList shall not be empty
    if null threadList
      then return $ FetchVoteThreadsForUserResponse []
      else do
        res <- fetchVoteThreadsByUser userIDForUserInfo threadList
        pure $
          FetchVoteThreadsForUserResponse $
            (\ThreadVote {..} -> (threadVoteThreadID, threadVote)) <$> res
fetchVoteThreadsForUserH _ _ = throw401Err "Please login first"
