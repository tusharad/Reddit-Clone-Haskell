{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Platform.Admin.Handler (
    adminDashboardH,
    adminChangePasswordH
) where

import Platform.Admin.Types
import Platform.Common.AppM
import Servant.Auth.Server
import Platform.Auth.Types
import Platform.DB.Model
import qualified Data.ByteString.Lazy.Char8 as BSL
import Platform.Common.Utils
import Platform.Admin.DB
import Data.Time
import qualified Data.Text as T
import Control.Monad (when)
import UnliftIO

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

adminChangePasswordH ::
    (MonadUnliftIO m) =>
    AuthResult AdminInfo ->
    AdminChangePasswordBody ->
    AppM m AdminChangePasswordResponse
adminChangePasswordH (Authenticated AdminInfo {..}) AdminChangePasswordBody {..} = do
    mAdmin <- fetchAdminByID adminIDForAdminInfo
    case mAdmin of
        Nothing -> throw400Err "Admin is invalid!"
        Just oldAdminData@Admin {adminPassword = aPassword} -> do
            checkOldPassword aPassword
            checkNewAndConfirmPassword
            validateNewPassword
            updateAdminPassword oldAdminData
            -- if aPassword /= oldPassword
            --     then throw400Err "Old password is incorrect!"
            --     else if newPassword /= confirmNewPassword
            --         then throw400Err "New password and confirm password do not match!"
            --         else do
            --             let newAdmin = Admin {adminPassword = newPassword}
            --             eRes <- try $ updateAdminPasswordQ adminIDForAdminInfo newAdmin
            --             case eRes of
            --                 Left e -> throw400Err $ BSL.pack $ show e
            --                 Right _ -> return AdminChangePasswordResponse
            --                     { adminChangePasswordRespMsg = "Password updated successfully!"
            --                     }
  where
    checkOldPassword aPassword =
        when (aPassword /= oldPassword) $ throw400Err "Old password is incorrect!"
    checkNewAndConfirmPassword =
        when (newPassword /= confirmNewPassword) $ throw400Err "New password and confirm password do not match!"
    validateNewPassword = 
        when (newPassword == oldPassword) $ throw400Err "New password should be different from old password!"
    updateAdminPassword oldPassword0 = do
        let newAdmin = oldPassword0 {
            adminPassword = newPassword,
            adminID = (),
            createdAtForAdmin = (),
            updatedAtForAdmin = ()
        }
        eRes :: Either SomeException () <- try $ updateAdminPasswordQ adminIDForAdminInfo newAdmin
        case eRes of
            Left e -> throw400Err $ BSL.pack $ show e
            Right _ -> return AdminChangePasswordResponse
                { adminChangePasswordRespMsg = "Password updated successfully!"
                }
    fetchAdminByID adminID0 = do
        (eRes :: Either SomeException (Maybe AdminRead)) <- try $ fetchAdminByIDQ adminID0
        case eRes of
            Left e -> throw400Err $ BSL.pack $ show e
            Right mAdmin -> pure mAdmin
adminChangePasswordH _ _ = throw401Err "Please login first"