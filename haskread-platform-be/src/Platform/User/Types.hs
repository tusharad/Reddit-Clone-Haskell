{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Platform.User.Types
  ( RegisterUserBody (..),
    RegisterUserResponse (..),
    LoginUserBody (..),
    LoginUserResponse (..),
    UserProfileResponse (..),
    ChangePasswordBody (..),
    ChangePasswordResponse (..)
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Platform.DB.Model

data RegisterUserBody = RegisterUserBody
  { userName :: Text,
    email :: Text,
    password :: Text,
    confirmPassword :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RegisterUserResponse = RegisterUserResponse
  { userIDForRUR :: UserID,
    registerUserResponseMessage :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data LoginUserBody = LoginUserBody
  { email :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data LoginUserResponse = LoginUserResponse
  { jwtToken :: Text,
    loginUserResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data UserProfileResponse = UserProfileResponse
  { userName :: Text,
    userEmail :: Text,
    joinedDate :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

data ChangePasswordBody = ChangePasswordBody
  { oldPassword :: Text,
    newPassword :: Text,
    confirmPassword :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ChangePasswordResponse = ChangePasswordResponse
  { changePasswordResponseMsg :: Text }
  deriving (Show, Eq, Generic, ToJSON)