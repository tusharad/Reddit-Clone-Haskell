{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Handler
  ( userDashboardH,
  )
where

import Data.ByteString.Lazy.Char8 as BSL
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Platform.User.DB
import Platform.User.Types
import Servant.Auth.Server
import UnliftIO

userDashboardH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  AppM m UserProfileResponse
userDashboardH (Authenticated UserInfo {..}) = do
  mUser <- fetchUserByID
  case mUser of
    Nothing -> throw400Err "User is invalid!"
    Just User {userName = uName, email = uEmail, createdAt = uCreatedAt} -> do
      return
        UserProfileResponse
          { userName = uName,
            userEmail = uEmail,
            joinedDate = uCreatedAt
          }
  where
    fetchUserByID = do
      eRes :: Either SomeException (Maybe UserRead) <- try $ fetchUserByIDQ userID
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right mUser -> pure mUser
userDashboardH _ = throw401Err "Please login first"
