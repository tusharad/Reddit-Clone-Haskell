{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ScottyCrud.Common.Types where

import GHC.Generics
import Database.PostgreSQL.Simple
import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock

data User = User {
    user_id    :: Int
  , user_email :: String
  , password :: String
} deriving (Show,Generic,FromRow,ToJSON)

data Post = Post {
    postId :: Int
  , postTitle :: T.Text
  , postDescription :: T.Text
  , userId   :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
} deriving (Show,Generic,Eq,FromRow)

data PostAndUser = PostAndUser {
    postId :: Int
  , postTitle :: T.Text
  , postDescription :: T.Text
  , userId   :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , userUserId :: Int
  , userUserEmail :: T.Text
  , userPassword :: T.Text
} deriving (Show,Generic,Eq,FromRow)

data Person = Person {
    id    :: Int
  , name :: String
  , age :: Int
} deriving (Show,Generic,FromRow,ToJSON)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "age"

data Person' = Person' {
    name2 :: String
  , age2 :: Int
} deriving (Show,Generic,ToRow)


instance FromJSON Person' where
    parseJSON = withObject "Person'" $ \v -> Person'
        <$> v .: "name"
        <*> v .: "age"



getConn :: IO Connection
getConn = connect defaultConnectInfo { connectHost = "localhost",connectDatabase="postgres",connectUser="tushar",connectPassword="1234" }
