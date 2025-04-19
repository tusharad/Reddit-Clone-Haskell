module Platform.User.DB
  ( fetchUserByEmailQ
  , fetchUserByUserNameQ
  , addUserQ
  , fetchUserByIDQ
  , updateUserQ
  , deleteUserQ
  , updateUserProfileImageQ
  , fetchUserProfileQ
  , addUserProfileImageQ
  , fetchUEVOByIDQ
  , deleteUEVO
  , deleteUEVOQ
  , fetchUEVOByID
  , addUEVOQ
  , fetchUserByID
  , updateUser
  , fetchUserByEmail
  , addUser
  , fetchUserProfilesQ 
  )
where

import Data.Text (Text)
import Orville.PostgreSQL
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import UnliftIO

fetchUserByEmail :: (MonadUnliftIO m) => Text -> AppM m (Maybe UserRead)
fetchUserByEmail = runQuery . fetchUserByEmailQ

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
addUser = runQuery . addUserQ

addUserQ :: (MonadOrville m) => UserWrite -> m UserRead
addUserQ = insertAndReturnEntity userTable

fetchUserByID :: (MonadUnliftIO m) => UserID -> AppM m (Maybe UserRead)
fetchUserByID = runQuery . fetchUserByIDQ 

fetchUserByIDQ :: (MonadOrville m) => UserID -> m (Maybe UserRead)
fetchUserByIDQ = findEntity userTable

updateUser :: (MonadUnliftIO m) => UserID -> UserWrite -> AppM m ()
updateUser userID0 userWrite0 = runQuery (updateUserQ userID0 userWrite0)

updateUserQ :: (MonadUnliftIO m) => UserID -> UserWrite -> AppM m ()
updateUserQ = updateEntity userTable

deleteUserQ :: (MonadUnliftIO m) => UserID -> AppM m ()
deleteUserQ = deleteEntity userTable

updateUserProfileImageQ :: (MonadUnliftIO m) => UserID -> UserProfileImageWrite -> AppM m ()
updateUserProfileImageQ = updateEntity userProfileImageTable

fetchUserProfileQ :: (MonadOrville m) => UserID -> m (Maybe UserProfileImageRead)
fetchUserProfileQ = findEntity userProfileImageTable

fetchUserProfilesQ :: MonadOrville m => m [UserProfileImageRead]
fetchUserProfilesQ = findEntitiesBy userProfileImageTable emptySelectOptions

addUserProfileImageQ :: (MonadUnliftIO m) => UserProfileImageWrite -> AppM m ()
addUserProfileImageQ = insertEntity userProfileImageTable

fetchUEVOByID :: (MonadUnliftIO m) => UserID -> AppM m (Maybe UserEmailVerifyOTPRead)
fetchUEVOByID = runQuery . fetchUEVOByIDQ

fetchUEVOByIDQ :: (MonadOrville m) => UserID -> m (Maybe UserEmailVerifyOTPRead)
fetchUEVOByIDQ = findEntity userEmailVerifyOTPTable

addUEVOQ :: (MonadOrville m) => UserEmailVerifyOTPWrite -> m ()
addUEVOQ = insertEntity userEmailVerifyOTPTable

deleteUEVO :: (MonadUnliftIO m) => UserID -> AppM m ()
deleteUEVO = runQuery . deleteUEVOQ

deleteUEVOQ :: (MonadOrville m) => UserID -> m ()
deleteUEVOQ = deleteEntity userEmailVerifyOTPTable
