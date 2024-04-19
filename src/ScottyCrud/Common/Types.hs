{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ScottyCrud.Common.Types where

import           GHC.Generics
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Data.Password.Bcrypt
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock
import qualified Data.Text.Encoding as ST

data User = User {
    user_id    :: Int
  , user_email :: String
  , password :: String
  , userName :: Text
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
    postId          :: Int
  , postTitle       :: Text
  , postDescription :: Text
  , userId          :: Int
  , createdAt       :: UTCTime
  , userName        :: Text
  , categoryName    :: Text
  , filePath        :: Maybe String
} deriving (Show,Generic,Eq,FromRow)

data CommentAndUser = CommentAndUser {
    commentId :: Int
  , commentContent :: Text
  , createdAt :: UTCTime
  , userId :: Int
  , userName :: Text
  , parentCommentId :: Maybe Int
  , postId :: Int
} deriving (Show,Generic,Eq,FromRow)

data NestedComment = NestedComment {
    mainComment :: CommentAndUser
  , childComments :: [NestedComment]
} deriving (Eq,Show)

getConn :: ConnectInfo
getConn = defaultConnectInfo { connectHost = "localhost",connectDatabase="postgres",connectUser="tushar",connectPassword="1234" }

instance ToField (PasswordHash Bcrypt) where
  toField = Escape . ST.encodeUtf8 . unPasswordHash
