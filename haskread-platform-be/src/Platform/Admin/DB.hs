module Platform.Admin.DB (
    findAdminByMailPasswordQ,
    fetchAdminByIDQ,
    updateAdminPasswordQ
) where

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
updateAdminPasswordQ  = updateEntity adminTable