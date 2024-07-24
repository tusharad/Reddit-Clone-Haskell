{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Handler
  ( userDashboardH,
  userChangePasswordH,
  userDeleteAccountH
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
import Control.Monad (when, unless)
import Platform.User.Types (DeleteUserResponse (DeleteUserResponse))

fetchUserByID uID0 = do
      eRes :: Either SomeException (Maybe UserRead) <- try $ fetchUserByIDQ uID0
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right mUser -> pure mUser

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
      eRes :: Either SomeException (Maybe UserRead) <- try $ fetchUserByIDQ userIDForUserInfo
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right mUser -> pure mUser
userDashboardH _ = throw401Err "Please login first"

userChangePasswordH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  ChangePasswordBody ->
  AppM m ChangePasswordResponse
userChangePasswordH (Authenticated UserInfo {..}) ChangePasswordBody {..} = do
  mUser <- fetchUserByID userIDForUserInfo
  case mUser of
    Nothing -> throw400Err "User is invalid!"
    Just u@User {password = uPassword} -> do
      checkOldPasswordMatch uPassword
      checkOldNewPasswordNotMatch
      checkIfPasswordsConfirmPasswordMatch
      validateNewPassword
      changePassword u
  where
    checkOldPasswordMatch uPassword =
      when (uPassword /= oldPassword) $ throw400Err "Old password is incorrect!"
    checkOldNewPasswordNotMatch =
      when (oldPassword == newPassword) $ throw400Err "Old password and new password cannot be the same!"
    checkIfPasswordsConfirmPasswordMatch =
      when (newPassword /= confirmPassword) $ throw400Err "New password and confirm password do not match!"
    validateNewPassword =
      unless (validatePassword newPassword) $ throw400Err passwordConstraintMessage
    changePassword u = do
      eRes :: Either SomeException () <- try $ changePasswordQ userIDForUserInfo (passwordUpdatedUser u newPassword)
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> return ChangePasswordResponse {changePasswordResponseMsg = "Password changed successfully!"}
userChangePasswordH _ _ = throw401Err "Please login first"

userDeleteAccountH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  DeleteUserBody ->
  AppM m DeleteUserResponse
userDeleteAccountH (Authenticated UserInfo {..}) DeleteUserBody{..} = do
  mUser <-fetchUserByID userIDForUserInfo
  case mUser of
    Nothing -> throw400Err "User is invalid!"
    Just User {password = uPassword} -> do
      checkIfPasswordMatch uPassword
      if not areUSure
        then pure $ DeleteUserResponse "User is not sure, not deleting :)"
      else deleteUserByID
  where
    checkIfPasswordMatch uPassword =
      when (uPassword /= passwordForDeleteUser) $ throw400Err "Password is incorrect!"
    deleteUserByID = do
      eRes :: Either SomeException () <- try $ deleteUserQ userIDForUserInfo
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> return $ DeleteUserResponse "Sad to see you go :L"
userDeleteAccountH _ _ = throw401Err "Please login first"