{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.User.Handler
  ( userDashboardH,
    userChangePasswordH,
    userDeleteAccountH,
    userUpdateProfileImageH,
    fetchUserProfileImageH,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Password.Bcrypt
import qualified Data.Text as T
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
import UnliftIO
import System.FilePath
import System.Directory
import System.Posix.Files (getFileStatus, fileSize)
import System.Posix.Types (COff(COff))

fetchUserProfileImageH ::
  (MonadUnliftIO m) =>
    UserID ->
    AppM m LBS.ByteString
fetchUserProfileImageH uId = do
   mbUserImage <- fetchUserProfileImage uId
   case mbUserImage of
    Nothing -> throw400Err "Not available"
    Just UserProfileImage{..} -> do
      let filePath = T.unpack userImage
      exists <- liftIO $ doesFileExist filePath
      if exists
        then do
          liftIO $ LBS.readFile filePath
        else throw400Err "Not available"

fetchUserByIDHaxl ::
  (MonadUnliftIO m) =>
  UserID ->
  AppM m (Maybe UserRead)
fetchUserByIDHaxl uID0 = do
  MyAppState
    { haxlConfig =
        HaxlConfig
          { pgConnectionPool = pool,
            numOfThreads = sem
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

{-
 eRes :: Either SomeException (Maybe UserRead) <- try $ fetchUserByIDQ uID0
 case eRes of
   Left e -> throw400Err $ BSL.pack $ show e
   Right mUser -> pure mUser
-}

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
          { userIDForUPR = coerce userIDForUserInfo,
            userNameForUPR = uName,
            userEmail = e,
            userCreatedAt = T.pack $ show c
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
      unless (matchPasswords
              uPassword
              oldPasswordForChangePass)
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
       unless (matchPasswords uPassword passwordForDeleteUser)
        $ throw400Err "Password is incorrect!"
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
  let (tempFP, fType, fName) = userImageInfo
  checkValidImageType fType
  mUserProfile <- fetchUserProfileImage userIDForUserInfo
  serverFilePath <- createServerFilePath 30000 tempFP fName
  imageSize <- liftIO $ getFileSize tempFP
  let maxSizeKB = 300
      maxSizeBytes = maxSizeKB * 1024
  when (imageSize > maxSizeBytes) $ 
    throw400Err $ BSL.pack $ "Image size exceeds " ++ show maxSizeKB ++ "KB limit (" ++ show (imageSize `div` 1024) ++ "KB)"
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
    getFileSize :: FilePath -> IO Integer
    getFileSize path = do
      stat <- getFileStatus path
      let (COff fSize) = fileSize stat
      return $ fromIntegral fSize
userUpdateProfileImageH _ _ = throw401Err "Please login first"
