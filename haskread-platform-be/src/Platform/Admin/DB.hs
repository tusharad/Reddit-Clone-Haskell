module Platform.Admin.DB
  ( fetchAdminByIDQ,
    updateAdminPasswordQ,
    fetchAdminByEmailQ,
    addAdminQ,
  )
where

import Data.Text (Text)
import Orville.PostgreSQL
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table

fetchAdminByIDQ :: (MonadOrville m) => AdminID -> m (Maybe AdminRead)
fetchAdminByIDQ = findEntity adminTable

updateAdminPasswordQ :: (MonadOrville m) => AdminID -> AdminWrite -> m ()
updateAdminPasswordQ = updateEntity adminTable

fetchAdminByEmailQ :: (MonadOrville m) => Text -> m (Maybe AdminRead)
fetchAdminByEmailQ email0 =
  findFirstEntityBy adminTable whereEmailIs
  where
    whereEmailIs = where_ $ emailField .== email0

addAdminQ :: (MonadOrville m) => AdminWrite -> m ()
addAdminQ = insertEntity adminTable
