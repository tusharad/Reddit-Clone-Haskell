module Platform.User.DB
  ( fetchUserByEmailQ,
    fetchUserByUserNameQ,
    addUserQ,
    findUserByMailPasswordQ,
  )
where

import Data.Text (Text)
import Orville.PostgreSQL
import Platform.Common.AppM
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import UnliftIO

fetchUserByEmailQ :: (MonadUnliftIO m) => Text -> AppM m (Maybe UserRead)
fetchUserByEmailQ email0 = findFirstEntityBy userTable whereEmailIs
  where
    whereEmailIs = where_ $ emailField .== email0

fetchUserByUserNameQ :: (MonadUnliftIO m) => Text -> AppM m (Maybe UserRead)
fetchUserByUserNameQ userName0 =
  findFirstEntityBy userTable whereUserNameIs
  where
    whereUserNameIs = where_ $ userNameField .== userName0

addUserQ :: (MonadUnliftIO m) => UserWrite -> AppM m UserRead
addUserQ userWrite0 = insertAndReturnEntity userTable userWrite0

findUserByMailPasswordQ :: (MonadUnliftIO m) => Text -> Text -> AppM m (Maybe UserRead)
findUserByMailPasswordQ email0 password0 =
  findFirstEntityBy userTable whereEmailPasswordIs
  where
    whereEmailPasswordIs = where_ $ emailField .== email0 .&& passwordField .== password0
