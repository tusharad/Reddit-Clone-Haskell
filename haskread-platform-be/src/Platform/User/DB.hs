module Platform.User.DB
  ( fetchUserByEmailQ,
    fetchUserByUserNameQ,
    addUserQ,
    fetchUserByIDQ,
    changePasswordQ,
    deleteUserQ,
    updateUserProfileImageQ,
    fetchUserProfileQ,
    addUserProfileImageQ,
  )
where

import Data.Text (Text)
import Orville.PostgreSQL
import Platform.Common.AppM
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import UnliftIO

fetchUserByEmailQ :: (MonadOrville m) => Text -> m (Maybe UserRead)
fetchUserByEmailQ email0 = findFirstEntityBy userTable whereEmailIs
  where
    whereEmailIs = where_ $ emailField .== email0

fetchUserByUserNameQ :: (MonadOrville m) => Text -> m (Maybe UserRead)
fetchUserByUserNameQ userName0 =
  findFirstEntityBy userTable whereUserNameIs
  where
    whereUserNameIs = where_ $ userNameField .== userName0

addUserQ :: (MonadOrville m) => UserWrite -> m UserRead
addUserQ = insertAndReturnEntity userTable

fetchUserByIDQ :: (MonadOrville m) => UserID -> m (Maybe UserRead)
fetchUserByIDQ = findEntity userTable

changePasswordQ :: (MonadUnliftIO m) => UserID -> UserWrite -> AppM m ()
changePasswordQ = updateEntity userTable

deleteUserQ :: (MonadUnliftIO m) => UserID -> AppM m ()
deleteUserQ = deleteEntity userTable

updateUserProfileImageQ :: (MonadUnliftIO m) => UserID -> UserProfileImageWrite -> AppM m ()
updateUserProfileImageQ = updateEntity userProfileImageTable

fetchUserProfileQ :: (MonadOrville m) => UserID -> m (Maybe UserProfileImageRead)
fetchUserProfileQ = findEntity userProfileImageTable

addUserProfileImageQ :: (MonadUnliftIO m) => UserProfileImageWrite -> AppM m ()
addUserProfileImageQ = insertEntity userProfileImageTable
