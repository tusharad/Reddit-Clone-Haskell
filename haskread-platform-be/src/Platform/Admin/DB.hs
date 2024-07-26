module Platform.Admin.DB
  ( findAdminByMailPasswordQ,
    fetchAdminByIDQ,
    updateAdminPasswordQ,
    fetchAdminByEmailQ,
    addAdminQ,
  )
where

import Data.Text (Text)
import Orville.PostgreSQL
import Platform.Common.AppM
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import UnliftIO

findAdminByMailPasswordQ :: (MonadUnliftIO m) => Text -> Text -> AppM m (Maybe AdminRead)
findAdminByMailPasswordQ email0 password0 =
  findFirstEntityBy adminTable whereEmailPasswordIs
  where
    whereEmailPasswordIs = where_ $ emailField .== email0 .&& passwordField .== password0

fetchAdminByIDQ :: (MonadUnliftIO m) => AdminID -> AppM m (Maybe AdminRead)
fetchAdminByIDQ = findEntity adminTable

updateAdminPasswordQ :: (MonadUnliftIO m) => AdminID -> AdminWrite -> AppM m ()
updateAdminPasswordQ = updateEntity adminTable

fetchAdminByEmailQ :: (MonadUnliftIO m) => Text -> AppM m (Maybe AdminRead)
fetchAdminByEmailQ email0 =
  findFirstEntityBy adminTable whereEmailIs
  where
    whereEmailIs = where_ $ emailField .== email0

addAdminQ :: (MonadOrville m) => AdminWrite -> m ()
addAdminQ = insertEntity adminTable
