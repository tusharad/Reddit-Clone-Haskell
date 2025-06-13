{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Platform.Common.Haxl (HaskReadReq(..), State (..)) where

import Platform.DB.Model
import Data.Text
import Haxl.Core (StateKey (State), ShowP(..))
import Data.Hashable (Hashable (hashWithSalt))
import Orville.PostgreSQL (ConnectionPool)
import Control.Concurrent (QSem)

data HaskReadReq a where
  GetUserByID :: UserID -> HaskReadReq (Maybe UserRead)
  GetUserByEmail :: Text -> HaskReadReq (Maybe UserRead)
  GetVoteThread :: UserID -> ThreadID -> HaskReadReq (Maybe ThreadVoteRead)
  GetCommunityByName :: Text -> HaskReadReq (Maybe CommunityRead)
  GetCommunityByID :: CommunityID -> HaskReadReq (Maybe CommunityRead)
  GetThreadByID :: ThreadID -> HaskReadReq (Maybe ThreadRead)
  GetCommentByID :: CommentID -> HaskReadReq (Maybe CommentRead)
  GetCommentVote :: CommentID -> UserID -> HaskReadReq (Maybe CommentVoteRead)
  GetAdminByID :: AdminID -> HaskReadReq (Maybe AdminRead)
  GetAdminByEmail :: Text -> HaskReadReq (Maybe AdminRead)
  GetUserByUserName :: Text -> HaskReadReq (Maybe UserRead)
  GetUserProfileImage :: UserID -> HaskReadReq (Maybe UserProfileImageRead)

deriving instance Eq (HaskReadReq a)

deriving instance Show (HaskReadReq a)

instance ShowP HaskReadReq where showp = show

instance Hashable (HaskReadReq a) where
  hashWithSalt s (GetUserByID userID0) = hashWithSalt s (2 :: Int, userID0)
  hashWithSalt s (GetUserByEmail userEmail) = hashWithSalt s (10 :: Int, userEmail)
  hashWithSalt s (GetVoteThread userID0 threadID0) =
    hashWithSalt s (10 :: Int, userID0, threadID0)
  hashWithSalt s (GetCommunityByName commName) = hashWithSalt s (10 :: Int, commName)
  hashWithSalt s (GetCommunityByID cID0) = hashWithSalt s (10 :: Int, cID0)
  hashWithSalt s (GetThreadByID tID0) = hashWithSalt s (10 :: Int, tID0)
  hashWithSalt s (GetCommentByID cID) = hashWithSalt s (10 :: Int, cID)
  hashWithSalt s (GetCommentVote cID uID) = hashWithSalt s (10 :: Int, cID, uID)
  hashWithSalt s (GetAdminByID aID) = hashWithSalt s (10 :: Int, aID)
  hashWithSalt s (GetAdminByEmail email0) = hashWithSalt s (10 :: Int, email0)
  hashWithSalt s (GetUserByUserName userName0) = hashWithSalt s (10 :: Int, userName0)
  hashWithSalt s (GetUserProfileImage userID0) = hashWithSalt s (10 :: Int, userID0)

instance StateKey HaskReadReq where
  data State HaskReadReq = HaskReadState ConnectionPool QSem
