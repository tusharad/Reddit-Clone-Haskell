{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Platform.User.Thread.Types
  ( CreateThreadReqBody (..),
    CreateThreadResponse (..),
    UpdateThreadReqBody (..),
    UpdateThreadResponse (..),
    DeleteThreadResponse (..),
    FetchAllThreadsResponse (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Platform.DB.Model
import Servant.Multipart
import qualified Data.Text as T

data CreateThreadReqBody = CreateThreadReqBody
  { threadTitleForCreate :: Text,
    threadDescriptionForCreate :: Maybe Text,
    threadCommunityIDForCreate :: CommunityID,
    threadAttachment :: Maybe (FileData Tmp)
  }
  deriving (Show, Eq, Generic)

instance FromMultipart Tmp CreateThreadReqBody where
  fromMultipart multipartData =
    CreateThreadReqBody <$> lookupInput "threadTitleForCreate" multipartData
                        <*> optionalInput "threadDescriptionForCreate" multipartData
                        <*> lookupInputCommunityID "threadCommunityIDForCreate" multipartData
                        <*> optionalInputFile "threadAttachment" multipartData

optionalInputFile :: Text -> MultipartData tag -> Either String (Maybe (FileData tag))
optionalInputFile name md = Right $ hush $ lookupFile name md

optionalInput :: Text -> MultipartData tag -> Either String (Maybe Text)
optionalInput name md = Right $ hush $ lookupInput name md

lookupInputCommunityID :: Text -> MultipartData tag -> Either String CommunityID
lookupInputCommunityID name md = CommunityID . readFromText <$> lookupInput name md

lookupInputThreadID :: Text -> MultipartData tag -> Either String ThreadID
lookupInputThreadID name md = ThreadID . readFromText <$> lookupInput name md

readFromText :: Read a => Text -> a
readFromText = read . T.unpack

newtype CreateThreadResponse = CreateThreadResponse
  { createThreadResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data UpdateThreadReqBody = UpdateThreadReqBody
  { threadIDForUpdate :: ThreadID,
    threadTitleForUpdate :: Text,
    threadDescriptionForUpdate :: Maybe Text,
    threadCommunityIDForUpdate :: CommunityID,
    threadAttachmentForUpdate :: Maybe (FileData Tmp)
  }
  deriving (Show, Eq, Generic)

instance FromMultipart Tmp UpdateThreadReqBody where
  fromMultipart multipartData =
    UpdateThreadReqBody <$> lookupInputThreadID "threadIDForUpdate" multipartData
                        <*> lookupInput "threadTitleForUpdate" multipartData
                        <*> optionalInput "threadDescriptionForUpdate" multipartData
                        <*> lookupInputCommunityID "threadCommunityIDForUpdate" multipartData
                        <*> optionalInputFile "threadAttachmentForUpdate" multipartData

newtype UpdateThreadResponse = UpdateThreadResponse
  { updateThreadResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

newtype DeleteThreadResponse = DeleteThreadResponse
  { deleteThreadResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data FetchAllThreadsResponse = FetchAllThreadsResponse
  { threadsCount :: Int,
    threads :: [ThreadInfo]
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just