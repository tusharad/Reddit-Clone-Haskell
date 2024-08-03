{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Handler
  ( userDashboardH,
    userChangePasswordH,
    userDeleteAccountH,
    userUpdateProfileImageH,
  )
where

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Password.Bcrypt
import qualified Data.Text as T
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Platform.User.DB
import Platform.User.Types
import Servant.Auth.Server
import UnliftIO

fetchUserByID ::
  (MonadUnliftIO m) =>
  UserID ->
  AppM m (Maybe UserRead)
fetchUserByID uID0 = do
  eRes :: Either SomeException (Maybe UserRead) <- try $ fetchUserByIDQ uID0
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right mUser -> pure mUser

fetchUserProfileImage ::
  (MonadUnliftIO m) =>
  UserID ->
  AppM m (Maybe UserProfileImageRead)
fetchUserProfileImage uID = do
  eRes :: Either SomeException (Maybe UserProfileImageRead) <- try $ fetchUserProfileQ uID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right mUserProfileInfo -> pure mUserProfileInfo

userDashboardH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  AppM m UserProfileResponse
userDashboardH (Authenticated UserInfo {..}) = do
  mUser <- fetchUserByID userIDForUserInfo
  case mUser of
    Nothing -> throw400Err "User is invalid!"
    Just User {userName = uName, email = uEmail, createdAt = uCreatedAt} -> do
      return
        UserProfileResponse
          { userNameForUPR = uName,
            userEmailForUPR = uEmail,
            joinedDateForUPR = uCreatedAt
          }
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
    Just u@User {userPassword = uPassword} -> do
      checkOldPasswordMatch uPassword
      checkOldNewPasswordNotMatch
      checkIfPasswordsConfirmPasswordMatch
      validateNewPassword
      changePassword u
  where
    checkOldPasswordMatch uPassword =
      when
        ( not $
            matchPasswords
              uPassword
              oldPasswordForChangePass
        )
        $ throw400Err "Old password is incorrect!"
    checkOldNewPasswordNotMatch =
      when (oldPasswordForChangePass == newPasswordForChangePass) $
        throw400Err
          "Old password and new password cannot be the same!"
    checkIfPasswordsConfirmPasswordMatch =
      when (newPasswordForChangePass /= confirmPasswordForChangePass) $
        throw400Err
          "New password and confirm password do not match!"
    validateNewPassword =
      unless (validatePassword newPasswordForChangePass) $
        throw400Err passwordConstraintMessage
    changePassword u = do
      hashedPass <- hashPassword (mkPassword newPasswordForChangePass)
      eRes :: Either SomeException () <-
        try $
          changePasswordQ
            userIDForUserInfo
            (passwordUpdatedUser u hashedPass)
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ ->
          return
            ChangePasswordResponse
              { changePasswordResponseMsg = "Password changed successfully!"
              }
userChangePasswordH _ _ = throw401Err "Please login first"

userDeleteAccountH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  DeleteUserBody ->
  AppM m DeleteUserResponse
userDeleteAccountH (Authenticated UserInfo {..}) DeleteUserBody {..} = do
  mUser <- fetchUserByID userIDForUserInfo
  case mUser of
    Nothing -> throw400Err "User is invalid!"
    Just User {userPassword = uPassword} -> do
      checkIfPasswordMatch uPassword
      if not areUSure
        then pure $ DeleteUserResponse "User is not sure, not deleting :)"
        else deleteUserByID
  where
    checkIfPasswordMatch uPassword =
      when
        ( not $
            matchPasswords
              uPassword
              passwordForDeleteUser
        )
        $ throw400Err "Password is incorrect!"
    deleteUserByID = do
      eRes :: Either SomeException () <- try $ deleteUserQ userIDForUserInfo
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> return $ DeleteUserResponse "Sad to see you go :L"
userDeleteAccountH _ _ = throw401Err "Please login first"

createServerFilePath ::
  (MonadUnliftIO m) =>
  FilePath ->
  T.Text ->
  AppM m FilePath
createServerFilePath tempFP fName = do
  (eRes :: Either SomeException BSL.ByteString) <- liftIO $ try $ BSL.readFile tempFP
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right content -> do
      checkImageSize content
      let serverFilePath = "./file-upload/" <> T.unpack fName
      liftIO $ BSL.writeFile serverFilePath content
      pure serverFilePath
  where
    checkImageSize imageContent = do
      unless (BSL.length imageContent < 1000000) $ throw400Err "Image to large :("

userUpdateProfileImageH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  UpdateUserImageBody ->
  AppM m UpdateUserImageResponse
userUpdateProfileImageH (Authenticated UserInfo {..}) UpdateUserImageBody {..} = do
  let (tempFP, fType, fName) = userImageInfo
  checkValidImageType fType
  mUserProfile <- fetchUserProfileImage userIDForUserInfo
  serverFilePath <- createServerFilePath tempFP fName
  let userProfileImage =
        UserProfileImage
          { userIDForProfileImage = userIDForUserInfo,
            userImage = T.pack serverFilePath,
            createdAtForProfileImage = (),
            updatedAtForProfileImage = ()
          }
  case mUserProfile of
    Nothing -> do
      addUserProfileImageQ userProfileImage
      pure $ UpdateUserImageResponse "Profile image added successfully!"
    Just _ -> do
      updateUserProfileImageQ userIDForUserInfo userProfileImage
      pure $ UpdateUserImageResponse "Profile image added successfully!"
  where
    checkValidImageType fType = do
      let validImageTypes = ["image/png", "image/jpeg", "image/jpg"]
      unless (fType `elem` validImageTypes) $ throw400Err "Invalid image type!"
userUpdateProfileImageH _ _ = throw401Err "Please login first"
