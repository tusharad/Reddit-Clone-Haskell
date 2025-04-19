{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Admin.Handler
  ( adminDashboardH
  , adminChangePasswordH
  , adminCreateAdminH
  )
where

import Control.Monad (void, when)
import Data.Password.Bcrypt
import qualified Data.Text as T
import Data.Time
import Platform.Admin.DB
import Platform.Admin.Types
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Types
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
        { adminName = aName
        , adminEmail = aEmail
        , createdAtForAdmin = aCreatedAt
        } -> do
        return
          AdminDashboardResponse
            { adminName = aName
            , adminEmailForAdminDashboard = aEmail
            , adminRole = aRole
            , createdDate = T.pack $ show (utctDay aCreatedAt)
            }
  where
    aRole = "Admin"
    fetchAdminByID :: (MonadUnliftIO m) => AdminID -> AppM m (Maybe AdminRead)
    fetchAdminByID = runQuery . fetchAdminByIDQ
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
    checkOldPassword :: (MonadUnliftIO m) => MyPassword -> AppM m ()
    checkOldPassword aPassword =
      when
        ( not $
            matchPasswords
              aPassword
              oldPassword
        )
        $ throw400Err "Old password is incorrect!"
    checkNewAndConfirmPassword =
      when (newPassword /= confirmNewPassword) $
        throw400Err "New password and confirm password do not match!"
    validateNewPassword =
      when (newPassword == oldPassword) $
        throw400Err "New password should be different from old password!"

    updateAdminPassword :: (MonadUnliftIO m) => AdminRead -> AppM m AdminChangePasswordResponse
    updateAdminPassword oldPasswordAdmin = do
      hashedPass <- MyPassword <$> hashPassword (mkPassword (newPassword))
      let newAdmin =
            oldPasswordAdmin
              { adminPassword = hashedPass
              , adminID = ()
              , createdAtForAdmin = ()
              , updatedAtForAdmin = ()
              }
      runQuery $ updateAdminPasswordQ adminIDForAdminInfo newAdmin
      return
        AdminChangePasswordResponse
          { adminChangePasswordRespMsg = "Password updated successfully!"
          }

    fetchAdminByID :: (MonadUnliftIO m) => AdminID -> AppM m (Maybe AdminRead)
    fetchAdminByID = runQuery . fetchAdminByIDQ
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
    checkIfAdminEmailAlreadyExists = runQuery $ fetchAdminByEmailQ adminEmailForCreate
    checkIfPasswordConfirms =
      when (adminPasswordForCreate /= adminConfirmPasswordForCreate) $
        throw400Err "password and confirm password don't match!"

    addAdmin :: (MonadUnliftIO m) => AppM m AdminCreateAdminResponse
    addAdmin = do
      hashedPass <- MyPassword <$> hashPassword (mkPassword adminPasswordForCreate)
      runQuery $
        addAdminQ $
          Admin
            { adminName = adminNameForCreate
            , adminEmail = adminEmailForCreate
            , adminPassword = hashedPass
            , adminID = ()
            , createdAtForAdmin = ()
            , updatedAtForAdmin = ()
            }
      pure $ AdminCreateAdminResponse "Admin sucessfully added!"
adminCreateAdminH _ _ = throw401Err "Please login first"
