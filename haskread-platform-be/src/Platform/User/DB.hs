module Platform.User.DB
  ( fetchUserByEmailQ,
    fetchUserByUserNameQ,
    addUserQ,
    fetchUserByIDQ,
    updateUserQ,
    deleteUserQ,
    updateUserProfileImageQ,
    fetchUserProfileQ,
    addUserProfileImageQ,
    fetchUEVOByIDQ,
    deleteUEVO,
    deleteUEVOQ,
    fetchUEVOByID,
    addUEVOQ,
    fetchUserByID,
    updateUser,
    fetchUserByEmail,
    addUser,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import Orville.PostgreSQL
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import UnliftIO

fetchUserByEmail :: (MonadUnliftIO m) => Text -> AppM m (Maybe UserRead)
fetchUserByEmail email0 = do
  eRes <- try $ fetchUserByEmailQ email0
  case eRes of
    Left err -> throw400Err $ BSL.pack (show (err :: SomeException))
    Right mUser -> pure mUser

fetchUserByEmailQ :: (MonadOrville m) => Text -> m (Maybe UserRead)
fetchUserByEmailQ email0 = findFirstEntityBy userTable whereEmailIs
  where
    whereEmailIs = where_ $ emailField .== email0

fetchUserByUserNameQ :: (MonadOrville m) => Text -> m (Maybe UserRead)
fetchUserByUserNameQ userName0 =
  findFirstEntityBy userTable whereUserNameIs
  where
    whereUserNameIs = where_ $ userNameField .== userName0

addUser :: (MonadUnliftIO m) => UserWrite -> AppM m UserRead
addUser uWrite = do
  eRes <- try $ addUserQ uWrite
  case eRes of
    Left err -> throw400Err $ BSL.pack (show (err :: SomeException))
    Right mUser -> pure mUser

addUserQ :: (MonadOrville m) => UserWrite -> m UserRead
addUserQ = insertAndReturnEntity userTable

fetchUserByID :: (MonadUnliftIO m) => UserID -> AppM m (Maybe UserRead)
fetchUserByID userID0 = do
  eRes <- try $ fetchUserByIDQ userID0
  case eRes of
    Left e -> throw400Err $ BSL.pack (show (e :: SomeException))
    Right mUser -> pure mUser

fetchUserByIDQ :: (MonadOrville m) => UserID -> m (Maybe UserRead)
fetchUserByIDQ = findEntity userTable

updateUser :: (MonadUnliftIO m) => UserID -> UserWrite -> AppM m ()
updateUser userID0 userWrite0 = queryWrapper (updateUserQ userID0 userWrite0)

updateUserQ :: (MonadUnliftIO m) => UserID -> UserWrite -> AppM m ()
updateUserQ = updateEntity userTable

deleteUserQ :: (MonadUnliftIO m) => UserID -> AppM m ()
deleteUserQ = deleteEntity userTable

updateUserProfileImageQ :: (MonadUnliftIO m) => UserID -> UserProfileImageWrite -> AppM m ()
updateUserProfileImageQ = updateEntity userProfileImageTable

fetchUserProfileQ :: (MonadOrville m) => UserID -> m (Maybe UserProfileImageRead)
fetchUserProfileQ = findEntity userProfileImageTable

addUserProfileImageQ :: (MonadUnliftIO m) => UserProfileImageWrite -> AppM m ()
addUserProfileImageQ = insertEntity userProfileImageTable

fetchUEVOByID :: (MonadUnliftIO m) => UserID -> AppM m (Maybe UserEmailVerifyOTPRead)
fetchUEVOByID uID = do
  eRes <- try $ fetchUEVOByIDQ uID
  case eRes of
    Left e -> throw400Err $ BSL.pack (show (e :: SomeException))
    Right mRes -> pure mRes

fetchUEVOByIDQ :: (MonadOrville m) => UserID -> m (Maybe UserEmailVerifyOTPRead)
fetchUEVOByIDQ = findEntity userEmailVerifyOTPTable

addUEVOQ :: (MonadOrville m) => UserEmailVerifyOTPWrite -> m ()
addUEVOQ = insertEntity userEmailVerifyOTPTable

deleteUEVO :: (MonadUnliftIO m) => UserID -> AppM m ()
deleteUEVO userID0 = do
  eRes <- try $ deleteUEVOQ userID0
  case eRes of
    Left e -> throw400Err $ BSL.pack (show (e :: SomeException))
    Right _ -> pure ()

deleteUEVOQ :: (MonadOrville m) => UserID -> m ()
deleteUEVOQ = deleteEntity userEmailVerifyOTPTable
