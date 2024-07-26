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
import Data.Time
import GHC.Generics
import Platform.DB.Model
import Servant.Multipart

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
  { userNameForUPR :: Text,
    userEmailForUPR :: Text,
    joinedDateForUPR :: UTCTime
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
  { userImageInfo :: (FilePath, TheFileType, TheFileName)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype UpdateUserImageResponse = UpdateUserImageResponse
  { updateUserImageResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

type TheFileType = Text

type TheFileName = Text

instance FromMultipart Tmp UpdateUserImageBody where
  fromMultipart multipartData =
    UpdateUserImageBody <$> getFileInfo (lookupFile "pic" multipartData)
    where
      getFileInfo (Left e) = Left e
      getFileInfo (Right fData) = Right $ (fdPayload fData, fdFileCType fData, fdFileName fData)
