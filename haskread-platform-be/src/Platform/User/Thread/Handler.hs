{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Thread.Handler
  ( createThreadH
  , updateThreadH
  , deleteThreadH
  , fetchAllThreadsH
  , fetchThreadH
  , fetchAllThreadsBySearchH
  )
where

import Control.Monad (void, when)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.Community.DB
import Platform.DB.Model
import Platform.User.Thread.DB
import Platform.User.Thread.Types
import Servant.Auth.Server
import UnliftIO

checkIfCommunityExists :: (MonadUnliftIO m) => CommunityID -> AppM m ()
checkIfCommunityExists cID = do
  eRes :: Either SomeException (Maybe CommunityRead) <-
    try $ fetchCommunityByIDQ cID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right mCommunity ->
      when (isNothing mCommunity) $
        throw400Err "Community does not exist!"

checkThreadTitleNotEmpty :: (MonadUnliftIO m) => T.Text -> AppM m ()
checkThreadTitleNotEmpty tTitle =
  when (T.null tTitle) $
    throw400Err "Thread title cannot be empty!"

addThread :: (MonadUnliftIO m) => UserID -> CreateThreadReqBody -> AppM m CreateThreadResponse
addThread userID CreateThreadReqBody {..} = do
  let threadWrite =
        Thread
          { threadTitle = threadTitleForCreate
          , threadDescription = threadDescriptionForCreate
          , threadCommunityID = threadCommunityIDForCreate
          , threadUserID = userID
          , threadCreatedAt = ()
          , threadUpdatedAt = ()
          , threadID = ()
          }
  (eRes :: Either SomeException ()) <- try $ addThreadQ threadWrite
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ -> return $ CreateThreadResponse "Thread added successfully!"

checkIfUserOwnsThread :: (MonadUnliftIO m) => ThreadID -> UserID -> AppM m ()
checkIfUserOwnsThread tID uID = do
  eRes :: Either SomeException (Maybe ThreadRead) <-
    try $ fetchThreadByIDQ tID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right mThread -> case mThread of
      Nothing -> throw400Err "Thread does not exist!"
      Just Thread {..} ->
        when (threadUserID /= uID) $
          throw400Err "You do not own this thread!"

createThreadH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  CreateThreadReqBody ->
  AppM m CreateThreadResponse
createThreadH (Authenticated UserInfo {..}) CreateThreadReqBody {..} = do
  void $ checkIfCommunityExists threadCommunityIDForCreate
  void $ checkThreadTitleNotEmpty threadTitleForCreate
  addThread userIDForUserInfo CreateThreadReqBody {..}
createThreadH _ _ = throw401Err "Please login first"

updateThreadH ::
  (MonadUnliftIO m) => AuthResult UserInfo -> UpdateThreadReqBody -> AppM m UpdateThreadResponse
updateThreadH (Authenticated UserInfo {..}) UpdateThreadReqBody {..} = do
  void $ checkIfCommunityExists threadCommunityIDForUpdate
  void $ checkThreadTitleNotEmpty threadTitleForUpdate
  void $ checkIfUserOwnsThread threadIDForUpdate userIDForUserInfo
  let threadWrite =
        Thread
          { threadTitle = threadTitleForUpdate
          , threadDescription = threadDescriptionForUpdate
          , threadCommunityID = threadCommunityIDForUpdate
          , threadUserID = userIDForUserInfo
          , threadCreatedAt = ()
          , threadUpdatedAt = ()
          , threadID = ()
          }
  (eRes :: Either SomeException ()) <- try $ updateThreadQ threadIDForUpdate threadWrite
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ -> return $ UpdateThreadResponse "Thread updated successfully!"
updateThreadH _ _ = throw401Err "Please login first"

deleteThreadH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  ThreadID ->
  AppM m DeleteThreadResponse
deleteThreadH (Authenticated UserInfo {..}) threadID = do
  void $ checkIfUserOwnsThread threadID userIDForUserInfo
  (eRes :: Either SomeException ()) <- try $ deleteThreadQ threadID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ -> return $ DeleteThreadResponse "Thread deleted successfully!"
deleteThreadH _ _ = throw401Err "Please login first"

fetchAllThreadsH ::
  (MonadUnliftIO m) =>
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  AppM m FetchAllThreadsResponse
fetchAllThreadsH mLimit mOffSet mCommunityId mUserId = do
  threadInfoList <-
    queryWrapper
      ( fetchThreadInfoQ (fromMaybe 10 mLimit) (fromMaybe 0 mOffSet) mCommunityId mUserId
      )
  return $ FetchAllThreadsResponse (length threadInfoList) threadInfoList

fetchThreadH :: (MonadUnliftIO m) => ThreadID -> AppM m ThreadInfo
fetchThreadH tID = do
  mThreadInfo <- queryWrapper $ fetchThreadInfoByIDQ tID
  case mThreadInfo of
    Nothing -> throw400Err "Thread not found"
    Just t -> pure t

fetchAllThreadsBySearchH :: MonadUnliftIO m => Maybe Text -> AppM m FetchAllThreadsResponse
fetchAllThreadsBySearchH Nothing = throw400Err "Please provide search term"
fetchAllThreadsBySearchH (Just searchTerm_) = do
  let searchTerm = T.strip searchTerm_
  if T.null searchTerm
    then throw400Err "Search term is empty"
    else do
      threadInfoList <- queryWrapper (fetchThreadInfoByTextQ (convertSearchTermToTerms searchTerm))
      return $ FetchAllThreadsResponse (length threadInfoList) threadInfoList

convertSearchTermToTerms :: Text -> Text
convertSearchTermToTerms txt = T.intercalate " & " (T.words txt)
