{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.User.Types
  ( RegisterUserBody (..),
    RegisterUserResponse (..),
    LoginUserBody (..),
    LoginUserResponse (..),
    UserProfileResponse (..),
    ChangePasswordBody (..),
    ChangePasswordResponse (..),
    DeleteUserBody (..),
    DeleteUserResponse (..),
    UpdateUserImageBody (..),
    UpdateUserImageResponse (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import GHC.Int (Int32)
import Platform.DB.Model
import Servant.Multipart
import qualified Data.ByteString.Lazy as BSL

data RegisterUserBody = RegisterUserBody
  { userNameForRegister :: Text,
    emailForRegister :: Text,
    passwordForRegister :: Text,
    confirmPasswordForRegister :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RegisterUserResponse = RegisterUserResponse
  { userIDForRUR :: UserID,
    registerUserResponseMessage :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data LoginUserBody = LoginUserBody
  { emailForLogin :: Text,
    passwordForLogin :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data LoginUserResponse = LoginUserResponse
  { jwtToken :: Text,
    loginUserResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data UserProfileResponse = UserProfileResponse
  { userIDForUPR :: Int32,
    userNameForUPR :: Text,
    userEmail :: Text,
    userCreatedAt :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data ChangePasswordBody = ChangePasswordBody
  { oldPasswordForChangePass :: Text,
    newPasswordForChangePass :: Text,
    confirmPasswordForChangePass :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ChangePasswordResponse = ChangePasswordResponse
  {changePasswordResponseMsg :: Text}
  deriving (Show, Eq, Generic, ToJSON)

data DeleteUserBody = DeleteUserBody
  { passwordForDeleteUser :: Text,
    areUSure :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype DeleteUserResponse = DeleteUserResponse
  { deleteUserResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

newtype UpdateUserImageBody = UpdateUserImageBody
  { userImageInfo :: (BSL.ByteString, TheFileType, TheFileName)
  }
  deriving (Show, Eq, Generic)

newtype UpdateUserImageResponse = UpdateUserImageResponse
  { updateUserImageResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

type TheFileType = Text

type TheFileName = Text

instance FromMultipart Mem UpdateUserImageBody where
  fromMultipart multipartData =
    UpdateUserImageBody <$> getFileInfo (lookupFile "pic" multipartData)
    where
      getFileInfo (Left e) = Left e
      getFileInfo (Right fData) = Right $ (fdPayload fData, fdFileCType fData, fdFileName fData)
