{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Platform.Haxl.DataSource () where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception as Exp
import Control.Monad (void)
import Platform.Common.Haxl
import Haxl.Core
import Orville.PostgreSQL (ConnectionPool, runOrville)
import Platform.Admin.DB
import Platform.Comment.DB
import Platform.Community.DB
import Platform.User.DB
import Platform.User.Thread.DB
import Platform.User.Thread.VoteThread.DB

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
    e <- Exp.try $ runOrville pool $ case req of
      GetUserByID userID0 -> fetchUserByIDQ userID0
      GetUserByEmail userEmail -> fetchUserByEmailQ userEmail
      GetVoteThread userID0 threadID0 -> fetchThreadVoteQ userID0 threadID0
      GetCommunityByName commName -> fetchCommunityByNameQ commName
      GetCommunityByID cID -> fetchCommunityByIDQ cID
      GetThreadByID tID -> fetchThreadByIDQ tID
      GetCommentByID cID -> fetchCommentByIDQ cID
      GetCommentVote cID uID -> fetchCommentVoteQ cID uID
      GetAdminByID aID -> fetchAdminByIDQ aID
      GetAdminByEmail email0 -> fetchAdminByEmailQ email0
      GetUserByUserName userName0 -> fetchUserByUserNameQ userName0
      GetUserProfileImage userID0 -> fetchUserProfileQ userID0
    case e of
      Left errMsg -> putFailure resVar (errMsg :: SomeException)
      Right res -> putSuccess resVar res
