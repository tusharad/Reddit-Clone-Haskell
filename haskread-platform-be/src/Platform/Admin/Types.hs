{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Platform.Admin.Types
  ( AdminLoginBodyReq (..),
    AdminLoginResponse (..),
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
  { adminLoginRespMsg :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)
