{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Platform.Admin.Types
  ( AdminLoginBodyReq (..),
    AdminLoginResponse (..),
    AdminDashboardResponse(..),
    AdminChangePasswordBody(..),
    AdminChangePasswordResponse(..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data AdminLoginBodyReq = AdminLoginBodyReq
  { adminEmailForLogin :: Text,
    adminPasswordForLogin :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

data AdminLoginResponse = AdminLoginResponse
  { 
      eToken :: Text
    , adminLoginRespMsg :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

data AdminDashboardResponse = AdminDashboardResponse
  { 
      adminName :: Text
    , adminEmailForAdminDashboard :: Text
    , adminRole :: Text
    , createdDate :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

data AdminChangePasswordBody = AdminChangePasswordBody
  { 
      oldPassword :: Text
    , newPassword :: Text
    , confirmNewPassword :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

newtype AdminChangePasswordResponse = AdminChangePasswordResponse
  { 
      adminChangePasswordRespMsg :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)