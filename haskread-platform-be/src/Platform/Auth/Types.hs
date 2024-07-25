{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Platform.Auth.Types
  ( UserInfo (..),
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
