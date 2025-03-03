{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Handler
  ( userDashboardH
  , userChangePasswordH
  , userDeleteAccountH
  , userUpdateProfileImageH
  , fetchUserProfileImageH
  )
where

import Control.Monad (unless, when)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Coerce (coerce)
import Data.Password.Bcrypt
import qualified Data.Text as T
import Google.Cloud.Storage.Bucket
import Haxl.Core (dataFetch, initEnv, runHaxl, stateEmpty, stateSet)
import qualified Haxl.Core as Haxl
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Types
import Platform.Common.Utils
import Platform.DB.Model
import Platform.Haxl.DataSource
import Platform.Log
import Platform.User.DB
import Platform.User.Types
import Servant.Auth.Server
import System.FilePath
import UnliftIO

fetchUserProfileImageH ::
  (MonadUnliftIO m) =>
  UserID ->
  AppM m LBS.ByteString
fetchUserProfileImageH uId = do
  mbUserImage <- fetchUserProfileImage uId
  case mbUserImage of
    Nothing -> throw400Err "Not available"
    Just UserProfileImage {..} -> do
      let bucketName = T.unpack userImage
          objectName = T.unpack userImageName
      eRes <- liftIO $ downloadObject bucketName objectName
      case eRes of
        Left err -> throw400Err $ "Could not download image: " <> (BSL.pack err)
        Right r -> pure r

fetchUserByIDHaxl ::
  (MonadUnliftIO m) =>
  UserID ->
  AppM m (Maybe UserRead)
fetchUserByIDHaxl uID0 = do
  MyAppState
    { haxlConfig =
      HaxlConfig
        { pgConnectionPool = pool
        , numOfThreads = sem
        }
    } <-
    ask
  let st = HaskReadState pool sem
  eRes :: Either SomeException (Maybe UserRead) <- liftIO $ do
    env0 <- initEnv (stateSet st stateEmpty) () :: IO (Haxl.Env () [Int])
    try $
      runHaxl
        env0
        (dataFetch (GetUserByID uID0))
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right r -> pure r

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
    Just User {userName = uName, email = e, createdAt = c} -> do
      return
        UserProfileResponse
          { userIDForUPR = coerce userIDForUserInfo
          , userNameForUPR = uName
          , userEmail = e
          , userCreatedAt = T.pack $ show c
          }
userDashboardH _ = throw401Err "Please login first"

userChangePasswordH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  ChangePasswordBody ->
  AppM m ChangePasswordResponse
userChangePasswordH (Authenticated UserInfo {..}) ChangePasswordBody {..} = do
  mUser <- fetchUserByIDHaxl userIDForUserInfo
  case mUser of
    Nothing -> throw400Err "User is invalid!"
    Just u@User {userPassword = mUPassword} -> do
      logDebug $ "changing user password of " <> T.pack (show u)
      case mUPassword of
        Nothing -> throw400Err "Password not setup in the first place"
        Just uPassword -> do
          checkOldPasswordMatch uPassword
          checkOldNewPasswordNotMatch
          checkIfPasswordsConfirmPasswordMatch
          validateNewPassword
          changePassword u
  where
    checkOldPasswordMatch uPassword =
      unless
        ( matchPasswords
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
      let newPassUsr = passwordUpdatedUser u hashedPass
      logDebug $ "new password user : " <> T.pack (show newPassUsr)
      eRes :: Either SomeException () <-
        try $
          updateUser
            userIDForUserInfo
            newPassUsr
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
  mUser <- fetchUserByIDHaxl userIDForUserInfo
  case mUser of
    Nothing -> throw400Err "User is invalid!"
    Just User {userPassword = mUPassword} -> do
      case mUPassword of
        Nothing -> throw400Err "Password not found"
        Just uPassword -> do
          checkIfPasswordMatch uPassword
          if not areUSure
            then pure $ DeleteUserResponse "User is not sure, not deleting :)"
            else deleteUserByID
  where
    checkIfPasswordMatch uPassword =
      unless (matchPasswords uPassword passwordForDeleteUser) $
        throw400Err "Password is incorrect!"
    deleteUserByID = do
      eRes :: Either SomeException () <- try $ deleteUserQ userIDForUserInfo
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> return $ DeleteUserResponse "Sad to see you go :L"
userDeleteAccountH _ _ = throw401Err "Please login first"

userUpdateProfileImageH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  UpdateUserImageBody ->
  AppM m UpdateUserImageResponse
userUpdateProfileImageH (Authenticated UserInfo {..}) UpdateUserImageBody {..} = do
  let (content, fType, fName) = userImageInfo
  checkValidImageType fType
  mUserProfile <- fetchUserProfileImage userIDForUserInfo
  let newUserProfileImageName = show userIDForUserInfo <> "_profile_" <> takeExtension (T.unpack fName)
  eRes <- liftIO $ uploadObject "haskread_vm_storage2" newUserProfileImageName content
  case eRes of
    Left err -> throw400Err $ "Error while uploading image: " <> (BSL.pack err)
    Right _ -> do
      let imageSize = BSL.length content
      let maxSizeKB = 300
          maxSizeBytes = maxSizeKB * 1024
      when (imageSize > maxSizeBytes) $
        throw400Err $
          BSL.pack $
            "Image size exceeds "
              ++ show maxSizeKB
              ++ "KB limit ("
              ++ show (imageSize `div` 1024)
              ++ "KB)"
      let userProfileImage =
            UserProfileImage
              { userIDForProfileImage = userIDForUserInfo
              , userImage = "haskread_vm_storage2" -- Temporary hardcoded bucket name
              , userImageName = T.pack newUserProfileImageName
              , createdAtForProfileImage = ()
              , updatedAtForProfileImage = ()
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
