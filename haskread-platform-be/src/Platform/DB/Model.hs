{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Platform.DB.Model
  ( UserID (..),
    User (..),
    UserRead,
    UserWrite,
    UserProfileImageWrite,
    UserProfileImageRead,
    UserProfileImage (..),
    Admin (..),
    AdminRead,
    AdminWrite,
    AdminID (..),
    Community(..),
    CommunityRead,
    CommunityWrite,
    CommunityID(..)
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import GHC.Int (Int32)
import Web.HttpApiData


newtype UserID = UserID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data User a b = User
  { userID :: a,
    userName :: Text,
    email :: Text,
    password :: Text,
    createdAt :: b,
    updatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type UserRead = User UserID UTCTime

type UserWrite = User () ()

data UserProfileImage a = UserProfileImage
  { userIDForProfileImage :: UserID,
    userImage :: Text,
    createdAtForProfileImage :: a,
    updatedAtForProfileImage :: a
  }
  deriving (Show, Eq, Generic, ToJSON)

type UserProfileImageWrite = UserProfileImage ()

type UserProfileImageRead = UserProfileImage UTCTime

newtype AdminID = AdminID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data Admin a b = Admin
  { adminID :: a,
    adminName :: Text,
    adminEmail :: Text,
    adminPassword :: Text,
    createdAtForAdmin :: b,
    updatedAtForAdmin :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type AdminRead = Admin AdminID UTCTime

type AdminWrite = Admin () ()

newtype CommunityID = CommunityID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON,FromHttpApiData)
data Community a b = Community
  { communityID :: a,
    communityName :: Text,
    communityDescription :: Text,
    communityLabelList :: Text,
    communityCreatedAt :: b,
    communityUpdatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type CommunityRead = Community CommunityID UTCTime
type CommunityWrite = Community () ()