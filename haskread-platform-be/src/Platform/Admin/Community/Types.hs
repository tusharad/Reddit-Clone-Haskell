{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Platform.Admin.Community.Types (
    CommunityCreateReqBody(..),
    CommunityCreateResponse(..),
    CommunityUpdateReqBody(..),
    CommunityUpdateResponse(..),
    CommunityDeleteResponse(..)
) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Platform.DB.Model

data CommunityCreateReqBody = CommunityCreateReqBody
  { communityNameForCreate :: Text,
    communityDescriptionForCreate :: Text,
    communityLabelListForCreate :: [Text]
  }
  deriving (Generic, Show, Eq,FromJSON)

newtype CommunityCreateResponse = CommunityCreateResponse {
    communityCreateResponseMsg :: Text
} deriving (Generic, Show, Eq, ToJSON)

data CommunityUpdateReqBody = CommunityUpdateReqBody
  { 
    communityIDForUpdate :: CommunityID,
    communityNameForUpdate :: Text,
    communityDescriptionForUpdate :: Text,
    communityLabelListForUpdate :: [Text]
  }
  deriving (Generic, Show, Eq,FromJSON)

newtype CommunityUpdateResponse = CommunityUpdateResponse {
    communityUpdateResponseMsg :: Text
} deriving (Generic, Show, Eq, ToJSON)

newtype CommunityDeleteResponse = CommunityDeleteResponse {
    communityDeleteResponseMsg :: Text
} deriving (Generic, Show, Eq, ToJSON)
