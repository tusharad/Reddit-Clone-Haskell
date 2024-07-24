{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Platform.DB.Model
  ( UserID (..),
    User (..),
    UserRead,
    UserWrite,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import GHC.Int (Int32)

newtype UserID = UserID Int32
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data User a b = User
  { userID :: a,
    userName :: Text,
    email :: Text,
    password :: Text,
    createdAt :: b,
    updatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type UserRead = User UserID UTCTime

type UserWrite = User () ()
