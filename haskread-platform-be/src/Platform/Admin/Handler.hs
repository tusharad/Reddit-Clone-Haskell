{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Platform.Admin.Handler (
    adminDashboardH
) where

import Platform.Admin.Types
import UnliftIO (MonadUnliftIO,try,SomeException)
import Platform.Common.AppM
import Servant.Auth.Server
import Platform.Auth.Types
import Platform.DB.Model
import qualified Data.ByteString.Lazy.Char8 as BSL
import Platform.Common.Utils
import Platform.Admin.DB
import Data.Time
import qualified Data.Text as T

adminDashboardH :: 
    (MonadUnliftIO m) =>
    AuthResult AdminInfo ->
    AppM m AdminDashboardResponse
adminDashboardH (Authenticated AdminInfo {..}) = do
    mAdmin <- fetchAdminByID adminIDForAdminInfo
    case mAdmin of 
        Nothing -> throw400Err "Admin is invalid!"
        Just Admin {adminName = aName
        , adminEmail = aEmail
        , createdAtForAdmin = aCreatedAt} -> do
            return AdminDashboardResponse
                { adminName = aName
                , adminEmailForAdminDashboard = aEmail
                , adminRole = aRole
                , createdDate = T.pack $ show (utctDay aCreatedAt)
                }
  where
    aRole = "Admin"
    fetchAdminByID adminID0 = do
        (eRes :: Either SomeException (Maybe AdminRead)) <- try $ fetchAdminByIDQ adminID0
        case eRes of
            Left e -> throw400Err $ BSL.pack $ show e
            Right mAdmin -> pure mAdmin
adminDashboardH _ = throw401Err "Please login first"