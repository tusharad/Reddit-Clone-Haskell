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
    Community (..),
    CommunityRead,
    CommunityWrite,
    CommunityID (..),
    Thread (..),
    ThreadRead,
    ThreadWrite,
    ThreadID (..),
    ThreadVote (..),
    ThreadVoteRead,
    ThreadVoteWrite,
    ThreadVoteID (..),
    Comment (..),
    CommentRead,
    CommentWrite,
    CommentID (..),
    CommentVoteID (..),
    CommentVote (..),
    CommentVoteRead,
    CommentVoteWrite,
    UserEmailVerifyOTP (..),
    UserEmailVerifyOTPRead,
    UserEmailVerifyOTPWrite,
    ThreadInfo (..),
  )
where

import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import GHC.Int (Int32)
import Platform.Common.Types (MyPassword)
import Web.HttpApiData

-- User Model
newtype UserID = UserID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Hashable, FromHttpApiData)

data User a b = User
  { userID :: a,
    userName :: Text,
    email :: Text,
    userPassword :: Maybe MyPassword,
    isUserVerified :: Bool,
    createdAt :: b,
    updatedAt :: b
  }
  deriving (Show, Generic, ToJSON)

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
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Hashable)

data Admin a b = Admin
  { adminID :: a,
    adminName :: Text,
    adminEmail :: Text,
    adminPassword :: MyPassword,
    createdAtForAdmin :: b,
    updatedAtForAdmin :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type AdminRead = Admin AdminID UTCTime

type AdminWrite = Admin () ()

-- Community Model
newtype CommunityID = CommunityID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData, Hashable)

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
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData, Hashable)

data Thread a b = Thread
  { threadID :: a,
    threadTitle :: Text,
    threadDescription :: Maybe Text,
    threadUserID :: UserID,
    threadCommunityID :: CommunityID,
    threadCreatedAt :: b,
    threadUpdatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type ThreadRead = Thread ThreadID UTCTime

type ThreadWrite = Thread () ()

-- ThreadVote Model

data ThreadVoteID = ThreadVoteID
  { threadVoteIDUserID :: UserID,
    threadVoteIDThreadID :: ThreadID
  }
  deriving (Show, Eq, Ord)

data ThreadVote a = ThreadVote
  { threadVoteUserID :: UserID,
    threadVoteThreadID :: ThreadID,
    threadVote :: Bool,
    threadVoteCreatedAt :: a,
    threadVoteUpdatedAt :: a
  }
  deriving (Show, Eq, Generic, ToJSON)

type ThreadVoteRead = ThreadVote UTCTime

type ThreadVoteWrite = ThreadVote ()

-- Comment Model

newtype CommentID = CommentID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData, Hashable)

data Comment a b = Comment
  { commentID :: a,
    userIDForComment :: UserID,
    threadIDForComment :: ThreadID,
    commentContent :: Text,
    parentCommentID :: Maybe CommentID,
    createdAtForComment :: b,
    updatedAtForComment :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type CommentRead = Comment CommentID UTCTime

type CommentWrite = Comment () ()

-- CommentVote Model

data CommentVoteID = CommentVoteID
  { commentVoteIDUserID :: UserID,
    commentVoteIDCommentID :: CommentID
  }
  deriving (Show, Eq, Ord)

data CommentVote a = CommentVote
  { userIDForCommentVote :: UserID,
    commentIDForCommentVote :: CommentID,
    commentVote :: Bool,
    createdAtForCommentVote :: a,
    updatedAtForCommentVote :: a
  }
  deriving (Show, Eq, Generic, ToJSON)

type CommentVoteRead = CommentVote UTCTime

type CommentVoteWrite = CommentVote ()

-- userEmailVerifyOTP

data UserEmailVerifyOTP a = UserEmailVerifyOTP
  { userIDForUEVO :: UserID,
    otpForUEVO :: Int32,
    createdAtForUEVO :: a
  }

type UserEmailVerifyOTPRead = UserEmailVerifyOTP UTCTime

type UserEmailVerifyOTPWrite = UserEmailVerifyOTP ()

-- Custom type to fetch data

data ThreadInfo = ThreadInfo
  { threadIDForThreadInfo :: ThreadID,
    title :: Text,
    description :: Maybe Text,
    createdAtForThreadInfo :: UTCTime,
    userIDForThreadInfo :: UserID,
    userNameForThreadInfo :: Text,
    communityIDForThreadInfo :: CommunityID,
    communityNameForThreadInfo :: Text,
    upvoteCount :: Maybe Int32,
    downvoteCount :: Maybe Int32,
    commentCount :: Maybe Int32
  }
  deriving (Show, Eq, Generic, ToJSON)
