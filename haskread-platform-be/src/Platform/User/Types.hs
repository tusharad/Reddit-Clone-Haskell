{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Platform.User.Types
  ( RegisterUserBody (..),
    RegisterUserResponse (..),
    LoginUserBody (..),
    LoginUserResponse (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Platform.DB.Model

data RegisterUserBody = RegisterUserBody
  { userName :: Text,
    email :: Text,
    password :: Text,
    confirmPassword :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

data RegisterUserResponse = RegisterUserResponse
  { userID :: UserID,
    registerUserResponseMessage :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data LoginUserBody = LoginUserBody
  { email :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

data LoginUserResponse = LoginUserResponse
  { jwtToken :: Text,
    loginUserResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)
