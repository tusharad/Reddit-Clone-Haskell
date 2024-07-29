{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Admin.Handler
  ( adminDashboardH,
    adminChangePasswordH,
    adminCreateAdminH,
  )
where

import Control.Monad (void, when)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Time
import Platform.Admin.DB
import Platform.Admin.Types
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Servant.Auth.Server
import UnliftIO

adminDashboardH ::
  (MonadUnliftIO m) =>
  AuthResult AdminInfo ->
  AppM m AdminDashboardResponse
adminDashboardH (Authenticated AdminInfo {..}) = do
  mAdmin <- fetchAdminByID adminIDForAdminInfo
  case mAdmin of
    Nothing -> throw400Err "Admin is invalid!"
    Just
      Admin
        { adminName = aName,
          adminEmail = aEmail,
          createdAtForAdmin = aCreatedAt
        } -> do
        return
          AdminDashboardResponse
            { adminName = aName,
              adminEmailForAdminDashboard = aEmail,
              adminRole = aRole,
              createdDate = T.pack $ show (utctDay aCreatedAt)
            }
  where
    aRole = "Admin"
    fetchAdminByID :: MonadUnliftIO m => AdminID -> AppM m (Maybe AdminRead)
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
  where
    checkOldPassword :: MonadUnliftIO m => T.Text -> AppM m ()
    checkOldPassword aPassword =
      when (aPassword /= oldPassword) $ throw400Err "Old password is incorrect!"
    checkNewAndConfirmPassword =
      when (newPassword /= confirmNewPassword) $ throw400Err "New password and confirm password do not match!"
    validateNewPassword =
      when (newPassword == oldPassword) $ throw400Err "New password should be different from old password!"
    
    updateAdminPassword :: MonadUnliftIO m => AdminRead -> AppM m AdminChangePasswordResponse
    updateAdminPassword oldPassword0 = do
      let newAdmin =
            oldPassword0
              { adminPassword = newPassword,
                adminID = (),
                createdAtForAdmin = (),
                updatedAtForAdmin = ()
              }
      eRes :: Either SomeException () <- try $ updateAdminPasswordQ adminIDForAdminInfo newAdmin
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ ->
          return
            AdminChangePasswordResponse
              { adminChangePasswordRespMsg = "Password updated successfully!"
              }

    fetchAdminByID :: MonadUnliftIO m => AdminID -> AppM m (Maybe AdminRead)
    fetchAdminByID adminID0 = do
      (eRes :: Either SomeException (Maybe AdminRead)) <- try $ fetchAdminByIDQ adminID0
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right mAdmin -> pure mAdmin
adminChangePasswordH _ _ = throw401Err "Please login first"

adminCreateAdminH ::
  (MonadUnliftIO m) =>
  AuthResult AdminInfo ->
  AdminCreateAdminReqBody ->
  AppM m AdminCreateAdminResponse
adminCreateAdminH (Authenticated _) AdminCreateAdminReqBody {..} = do
  void $ checkIfAdminEmailAlreadyExists
  void $ checkIfPasswordConfirms
  addAdmin
  where
    checkIfAdminEmailAlreadyExists = do
      eRes :: Either SomeException (Maybe AdminRead) <-
        try $ fetchAdminByEmailQ adminEmailForCreate
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r
    checkIfPasswordConfirms =
      when (adminPasswordForCreate /= adminConfirmPasswordForCreate) $
        throw400Err "password and confirm password don't match!"

    addAdmin :: MonadUnliftIO m => AppM m AdminCreateAdminResponse
    addAdmin = do
      eRes :: Either SomeException () <-
        try $
          addAdminQ $
            Admin
              { adminName = adminNameForCreate,
                adminEmail = adminEmailForCreate,
                adminPassword = adminPasswordForCreate,
                adminID = (),
                createdAtForAdmin = (),
                updatedAtForAdmin = ()
              }
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> pure $ AdminCreateAdminResponse "Admin sucessfully added!"
adminCreateAdminH _ _ = throw401Err "Please login first"
