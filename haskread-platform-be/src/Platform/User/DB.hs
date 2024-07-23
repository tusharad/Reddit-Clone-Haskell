module Platform.User.DB (
    fetchUserByEmailQ
  , fetchUserByUserNameQ
  , addUserQ
) where

import Platform.Common.AppM
import Data.Text (Text)
import Platform.DB.Table
import Platform.DB.Marshaller
import Platform.DB.Model

import Orville.PostgreSQL
import UnliftIO

fetchUserByEmailQ :: MonadUnliftIO m => Text -> AppM m (Maybe UserRead)
fetchUserByEmailQ email0 = findFirstEntityBy userTable whereEmailIs
  where
    whereEmailIs = where_ $ emailField .== email0

fetchUserByUserNameQ :: MonadUnliftIO m => Text -> AppM m (Maybe UserRead)
fetchUserByUserNameQ userName0 =
    findFirstEntityBy userTable whereUserNameIs
  where
    whereUserNameIs = where_ $ userNameField .== userName0

addUserQ :: MonadUnliftIO m => UserWrite -> AppM m UserRead
addUserQ userWrite0 = insertAndReturnEntity userTable userWrite0
