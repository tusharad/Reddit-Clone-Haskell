{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Platform.User.Types (
    RegisterUserBody(..)
  , RegisterUserResponse(..)
) where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Platform.DB.Model

data RegisterUserBody = RegisterUserBody {
     userName :: Text
    , email :: Text
    , password :: Text
    , confirmPassword :: Text
} deriving (Show,Eq,Generic,FromJSON)

data RegisterUserResponse = RegisterUserResponse {
    userID :: UserID
 ,  registerUserResponseMessage :: Text
} deriving (Show,Eq,Generic,ToJSON)
