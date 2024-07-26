{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Platform.Admin.Types
  ( AdminLoginBodyReq (..),
    AdminLoginResponse (..),
    AdminDashboardResponse(..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data AdminLoginBodyReq = AdminLoginBodyReq
  { adminEmail :: Text,
    adminPassword :: Text
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