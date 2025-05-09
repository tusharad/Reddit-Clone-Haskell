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
  , fetchThreadAttachmentH
  )
where

import Control.Monad (unless, void, when)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Data.UUID.V4
import Google.Cloud.Storage.Bucket
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.Community.DB
import Platform.DB.Model
import Platform.User.Thread.DB
import Platform.User.Thread.Types
import Servant.Auth.Server
import Servant.Multipart
import System.FilePath
import UnliftIO

fetchThreadAttachmentH ::
  (MonadUnliftIO m) =>
  ThreadID ->
  AppM m LBS.ByteString
fetchThreadAttachmentH tId = do
  mbThreadRead <- runQuery $ fetchThreadByIDQ tId
  case mbThreadRead of
    Nothing ->
      throw400Err $
        "No thread found with given threadId " <> (TE.encodeUtf8 . TL.pack $ show tId)
    Just Thread {..} -> do
      case (threadAttachment, threadAttachmentName) of
        (Just attachmentPath, Just attachmentName_) -> do
          let bucketName = T.unpack attachmentPath
              objectName = T.unpack attachmentName_
          eRes <- liftIO $ downloadObject bucketName objectName
          case eRes of
            Left err -> throw400Err $ "Could not download attachment: " <> (BSL.pack err)
            Right r -> pure r
        _ ->
          throw400Err $ "No attachment found: " <> (BSL.pack $ show (threadAttachment, threadAttachmentName))

checkIfCommunityExists :: (MonadUnliftIO m) => CommunityID -> AppM m ()
checkIfCommunityExists cID = do
  mCommunity <- runQuery $ fetchCommunityByIDQ cID
  when (isNothing mCommunity) $
    throw400Err "Community does not exist!"

checkThreadTitleNotEmpty :: (MonadUnliftIO m) => T.Text -> AppM m ()
checkThreadTitleNotEmpty tTitle =
  when (T.null tTitle) $
    throw400Err "Thread title cannot be empty!"

data AttachmentInfo = AttachmentInfo
  { serverFilePath :: FilePath
  , attachmentName_ :: Text
  , attachmentSize_ :: Int
  }

addThread :: (MonadUnliftIO m) => UserID -> CreateThreadReqBody -> AppM m CreateThreadResponse
addThread userID CreateThreadReqBody {..} = do
  mbAttachmentInfo <- storeAttachmentIfExist threadAttachment
  let threadWrite =
        Thread
          { threadTitle = threadTitleForCreate
          , threadDescription = threadDescriptionForCreate
          , threadCommunityID = threadCommunityIDForCreate
          , threadAttachment = T.pack <$> serverFilePath <$> mbAttachmentInfo
          , threadAttachmentName = attachmentName_ <$> mbAttachmentInfo
          , threadAttachmentSize = fromIntegral . attachmentSize_ <$> mbAttachmentInfo
          , threadUserID = userID
          , threadCreatedAt = ()
          , threadUpdatedAt = ()
          , threadID = ()
          }
  runQuery $ addThreadQ threadWrite
  return $ CreateThreadResponse "Thread added successfully!"

supportedFileTypes :: [Text]
supportedFileTypes =
  [ "application/zip"
  , "application/x-zip-compressed"
  , "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  , "application/vnd.ms-excel"
  , "text/plain"
  , "application/x-tar"
  , "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  , "application/vnd.ms-powerpoint"
  , "application/pdf"
  , "image/png"
  , "application/vnd.oasis.opendocument.text"
  , "image/jpeg"
  ]

storeAttachmentIfExist :: MonadUnliftIO m => Maybe (FileData Mem) -> AppM m (Maybe AttachmentInfo)
storeAttachmentIfExist Nothing = pure Nothing
storeAttachmentIfExist (Just FileData {..}) = do
  -- Checking total threads...if number of threads crossed 10000 stop
  totalThreads <- fetchAllThreads
  if (length totalThreads >= 10000)
    then
      throw400Err "Threads fulled!"
    else do
      if fdFileCType `notElem` supportedFileTypes
        then
          throw400Err "File type not supported"
        else do
          uuid <- liftIO $ nextRandom
          let newAttachmentObjectName = "attachment_" <> show uuid <> takeExtension (T.unpack fdFileName)
          let fileSize = BSL.length fdPayload
          unless (fileSize < 1000000) $ throw400Err "File to large :("
          eRes <- liftIO $ uploadObject "haskread_vm_storage2" newAttachmentObjectName fdPayload
          case eRes of
            Left err -> throw400Err $ "Could not upload object: " <> (BSL.pack err)
            Right _ ->
              pure $
                Just
                  ( AttachmentInfo
                      "haskread_vm_storage2"
                      (T.pack newAttachmentObjectName)
                      (fromIntegral fileSize)
                  )

checkIfUserOwnsThread :: (MonadUnliftIO m) => ThreadID -> UserID -> AppM m ThreadRead
checkIfUserOwnsThread tID uID = do
  mThread <- runQuery $ fetchThreadByIDQ tID
  case mThread of
    Nothing -> throw400Err "Thread does not exist!"
    Just t@Thread {..} -> do
      when (threadUserID /= uID) $
        throw400Err "You do not own this thread!"
      pure t

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
  mbAttachmentInfo <- storeAttachmentIfExist threadAttachmentForUpdate
  let threadWrite =
        Thread
          { threadTitle = threadTitleForUpdate
          , threadDescription = threadDescriptionForUpdate
          , threadCommunityID = threadCommunityIDForUpdate
          , threadUserID = userIDForUserInfo
          , threadAttachment = T.pack <$> serverFilePath <$> mbAttachmentInfo
          , threadAttachmentName = attachmentName_ <$> mbAttachmentInfo
          , threadAttachmentSize = fromIntegral . attachmentSize_ <$> mbAttachmentInfo
          , threadCreatedAt = ()
          , threadUpdatedAt = ()
          , threadID = ()
          }
  runQuery $ updateThreadQ threadIDForUpdate threadWrite
  return $ UpdateThreadResponse "Thread updated successfully!"
updateThreadH _ _ = throw401Err "Please login first"

deleteThreadH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  ThreadID ->
  AppM m DeleteThreadResponse
deleteThreadH (Authenticated UserInfo {..}) _threadID = do
  Thread {..} <- checkIfUserOwnsThread _threadID userIDForUserInfo
  case (threadAttachment, threadAttachmentName) of
    (Just bucketName, Just objectName) -> do
      eRes <- liftIO $ deleteObject (T.unpack bucketName) (T.unpack objectName)
      case eRes of
        Left err -> throw400Err $ "Couldn't delete attachment please reach out :" <> (BSL.pack err)
        Right _ -> pure ()
    _ -> pure ()
  runQuery $ deleteThreadQ threadID
  return $ DeleteThreadResponse "Thread deleted successfully!"
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
    runQuery
      ( fetchThreadInfoQ (fromMaybe 10 mLimit) (fromMaybe 0 mOffSet) mCommunityId mUserId
      )
  return $ FetchAllThreadsResponse (length threadInfoList) threadInfoList

fetchThreadH :: (MonadUnliftIO m) => ThreadID -> AppM m ThreadInfo
fetchThreadH tID = do
  mThreadInfo <- runQuery $ fetchThreadInfoByIDQ tID
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
      threadInfoList <- runQuery (fetchThreadInfoByTextQ (convertSearchTermToTerms searchTerm))
      return $ FetchAllThreadsResponse (length threadInfoList) threadInfoList

convertSearchTermToTerms :: Text -> Text
convertSearchTermToTerms txt = T.intercalate " & " (T.words txt)
