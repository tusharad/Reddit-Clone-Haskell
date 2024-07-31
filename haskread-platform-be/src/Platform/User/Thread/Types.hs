{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Platform.User.Thread.Types (
 CreateThreadReqBody(..),
 CreateThreadResponse(..),
 UpdateThreadReqBody(..),
 UpdateThreadResponse(..),
 DeleteThreadResponse(..)
) where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Platform.DB.Model


data CreateThreadReqBody = CreateThreadReqBody {   
    threadTitleForCreate :: Text
  , threadDescriptionForCreate :: Maybe Text
  , threadCommunityIDForCreate :: CommunityID
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype CreateThreadResponse = CreateThreadResponse {
    createThreadResponseMsg :: Text
} deriving (Show, Eq, Generic, ToJSON)

data UpdateThreadReqBody = UpdateThreadReqBody {
    threadIDForUpdate :: ThreadID
  , threadTitleForUpdate :: Text
  , threadDescriptionForUpdate :: Maybe Text
  , threadCommunityIDForUpdate :: CommunityID
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON) 

newtype UpdateThreadResponse = UpdateThreadResponse {
    updateThreadResponseMsg :: Text
} deriving (Show, Eq, Generic, ToJSON)

newtype DeleteThreadResponse = DeleteThreadResponse {
    deleteThreadResponseMsg :: Text
} deriving (Show, Eq, Generic, ToJSON)