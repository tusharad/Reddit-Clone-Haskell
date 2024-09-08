{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Platform.Community.Types
  ( CommunityCreateReqBody (..),
    CommunityCreateResponse (..),
    CommunityUpdateReqBody (..),
    CommunityUpdateResponse (..),
    CommunityDeleteResponse (..),
    FetchCommunitiesResponse (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Platform.DB.Model

data CommunityCreateReqBody = CommunityCreateReqBody
  { communityNameForCreate :: Text,
    communityDescriptionForCreate :: Text,
    communityLabelListForCreate :: [Text]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

newtype CommunityCreateResponse = CommunityCreateResponse
  { communityCreateResponseMsg :: Text
  }
  deriving (Generic, Show, Eq, ToJSON)

data CommunityUpdateReqBody = CommunityUpdateReqBody
  { communityIDForUpdate :: CommunityID,
    communityNameForUpdate :: Text,
    communityDescriptionForUpdate :: Text,
    communityLabelListForUpdate :: [Text]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

newtype CommunityUpdateResponse = CommunityUpdateResponse
  { communityUpdateResponseMsg :: Text
  }
  deriving (Generic, Show, Eq, ToJSON)

newtype CommunityDeleteResponse = CommunityDeleteResponse
  { communityDeleteResponseMsg :: Text
  }
  deriving (Generic, Show, Eq, ToJSON)

data FetchCommunitiesResponse = FetchCommunitiesResponse
  { communities :: [CommunityRead],
    communityCount :: Int
  }
  deriving (Generic, Show, Eq, ToJSON)
