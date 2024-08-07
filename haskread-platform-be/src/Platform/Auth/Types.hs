{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Platform.Auth.Types
  ( UserInfo (..),
    AdminInfo (..),
    VerifyEmailResponse (..),
    ResendVerifyEmailResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics
import Platform.DB.Model
import Servant.Auth.Server

data UserInfo = UserInfo
  { userIDForUserInfo :: UserID,
    userNameForUserInfo :: Text
  }
  deriving (Show, Eq, Generic, ToJWT, ToJSON, FromJSON, FromJWT)

data AdminInfo = AdminInfo
  { adminIDForAdminInfo :: AdminID,
    adminNameForAdminInfo :: Text
  }
  deriving (Show, Eq, Generic, ToJWT, ToJSON, FromJSON, FromJWT)

newtype VerifyEmailResponse = VerifyEmailResponse Text
  deriving (Show, Eq, Generic, ToJSON)

newtype ResendVerifyEmailResponse = ResendVerifyEmailResponse Text
  deriving (Show, Eq, Generic, ToJSON)
