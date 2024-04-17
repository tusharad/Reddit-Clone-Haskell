{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ScottyCrud.Common.Types where

import           GHC.Generics
import           Database.PostgreSQL.Simple
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock

data User = User {
    user_id    :: Int
  , user_email :: String
  , password :: String
} deriving (Show,Generic,FromRow,ToJSON)

data Post = Post {
    postId :: Int
  , postTitle :: Text
  , postDescription :: Text
  , userId   :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , category_id :: Int
} deriving (Show,Generic,Eq,FromRow)

data PostAndUserAndCat = PostAndUserAndCat {
    postId :: Int
  , postTitle :: Text
  , postDescription :: Text
  , userId   :: Int
  , createdAt :: UTCTime
  , userUserEmail :: Text
  , categoryName :: Text
} deriving (Show,Generic,Eq,FromRow)

data CommentAndUser = CommentAndUser {
    commentId :: Int
  , commentContent :: Text
  , createdAt :: UTCTime
  , userId :: Int
  , userEmail :: Text
  , parentCommentId :: Maybe Int
  , postId :: Int
} deriving (Show,Generic,Eq,FromRow)

data NestedComment = NestedComment {
    mainComment :: CommentAndUser
  , childComments :: [NestedComment]
} deriving (Eq,Show)

getConn :: IO Connection
getConn = connect defaultConnectInfo { connectHost = "localhost",connectDatabase="postgres",connectUser="tushar",connectPassword="1234" }
