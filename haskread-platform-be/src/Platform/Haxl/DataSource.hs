{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Platform.Haxl.DataSource
  ( HaskReadReq (..),
    State (..),
  )
where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception as Exp
import Control.Monad (void)
import Data.Hashable
import Data.Text (Text)
import Haxl.Core
import Orville.PostgreSQL (ConnectionPool, runOrville)
import Platform.Community.DB
import Platform.Admin.DB
import Platform.Comment.DB
import Platform.DB.Model
import Platform.User.DB
import Platform.User.Thread.DB
import Platform.User.Thread.VoteThread.DB

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

instance DataSourceName HaskReadReq where
  dataSourceName _ = "HaskRead"

instance DataSource u HaskReadReq where
  fetch = haskReadFetch

haskReadFetch :: State HaskReadReq -> Flags -> u -> PerformFetch HaskReadReq
haskReadFetch (HaskReadState pool threads) _ _ =
  BackgroundFetch $ mapM_ (asyncFunc threads pool)

asyncFunc :: QSem -> ConnectionPool -> BlockedFetch HaskReadReq -> IO ()
asyncFunc sem pool (BlockedFetch req resVar) =
  void $ async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    (e :: Either SomeException a) <-
      Exp.try $
        runOrville pool
          =<< case req of
            GetUserByID userID0 -> pure $ fetchUserByIDQ userID0
            GetUserByEmail userEmail -> pure $ fetchUserByEmailQ userEmail
            GetVoteThread userID0 threadID0 ->
              pure $ fetchThreadVoteQ userID0 threadID0
            GetCommunityByName commName ->
              pure $ fetchCommunityByNameQ commName
            GetCommunityByID cID ->
              pure $ fetchCommunityByIDQ cID
            GetThreadByID tID ->
              pure $ fetchThreadByIDQ tID
            GetCommentByID cID ->
              pure $ fetchCommentByIDQ cID
            GetCommentVote cID uID ->
              pure $ fetchCommentVoteQ cID uID
            GetAdminByID aID ->
              pure $ fetchAdminByIDQ aID
            GetAdminByEmail email0 ->
              pure $ fetchAdminByEmailQ email0
            GetUserByUserName userName0 ->
              pure $ fetchUserByUserNameQ userName0
            GetUserProfileImage userID0 ->
              pure $ fetchUserProfileQ userID0

    case e of
      Left errMsg -> putFailure resVar errMsg
      Right res -> putSuccess resVar res
