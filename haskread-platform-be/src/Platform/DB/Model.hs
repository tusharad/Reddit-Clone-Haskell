{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Platform.DB.Model (
    UserID(..)
  , User(..)
  , UserRead
  , UserWrite
) where

import Data.Text (Text)
import GHC.Int (Int32)
import Data.Time (UTCTime)
import Data.Aeson

newtype UserID = UserID Int32 deriving (Show,Eq,Ord,ToJSON)

data User a b = User {
    userID :: a
  , userName :: Text
  , email :: Text
  , password :: Text
  , createdAt :: b
  , updatedAt :: b
} deriving (Show,Eq)

type UserRead = User UserID UTCTime
type UserWrite = User () ()
