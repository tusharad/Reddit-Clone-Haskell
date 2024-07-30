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
    CommunityID(..),
    Thread(..),
    ThreadRead,
    ThreadWrite,
    ThreadID(..),
    ThreadVote(..),
    ThreadVoteRead,
    ThreadVoteWrite,
    ThreadVoteID(..)
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import GHC.Int (Int32)
import Web.HttpApiData

-- User Model
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


-- User Profile Model
data UserProfileImage a = UserProfileImage
  { userIDForProfileImage :: UserID,
    userImage :: Text,
    createdAtForProfileImage :: a,
    updatedAtForProfileImage :: a
  }
  deriving (Show, Eq, Generic, ToJSON)

type UserProfileImageWrite = UserProfileImage ()

type UserProfileImageRead = UserProfileImage UTCTime

-- Admin Model
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

-- Community Model
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

-- Thread

newtype ThreadID = ThreadID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON,FromHttpApiData)

data Thread a b = Thread {
    threadID :: a
  , threadTitle :: Text
  , threadDescription :: Maybe Text
  , threadUserID :: UserID
  , threadCommunityID :: CommunityID
  , threadCreatedAt :: b
  , threadUpdatedAt :: b
} deriving (Show, Eq, Generic, ToJSON)

type ThreadRead = Thread ThreadID UTCTime
type ThreadWrite = Thread () ()

-- ThreadVote Model

data ThreadVoteID = ThreadVoteID {
    threadVoteIDUserID :: UserID 
  , threadVoteIDThreadID :: ThreadID
}
  deriving (Show, Eq, Ord)

data ThreadVote a = ThreadVote {
    threadVoteUserID :: UserID
  , threadVoteThreadID :: ThreadID
  , vote :: Bool
  , threadVoteCreatedAt :: a
  , threadVoteUpdatedAt :: a
} deriving (Show, Eq, Generic, ToJSON)

type ThreadVoteRead = ThreadVote UTCTime
type ThreadVoteWrite = ThreadVote ()