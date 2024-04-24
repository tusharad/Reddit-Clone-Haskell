{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ScottyCrud.Common.Types where

import           GHC.Generics
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Data.Password.Bcrypt
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock
import qualified Data.Text.Encoding as ST
import           Control.Monad.Reader
import           Control.Monad.IO.Unlift (MonadUnliftIO(..))

data DBSetting = DBSetting {
    host          :: String
  , schema        :: Text
  , dbPassword      :: String
  , user          :: String
  , databaseName  :: String
} deriving (Show,Generic,Read,Eq,FromJSON)

data AppSetting = AppSetting {
    port :: Int
  , dbSetting :: DBSetting
  , staticPath :: FilePath
  , uploadPath :: FilePath
  , mailerSendAPIToken :: String
  , mailerSendFromEmail :: Text
} deriving (Show,Generic,Read,Eq,FromJSON)

newtype AppM a = AppM { runAppM :: ReaderT AppSetting IO a}
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadReader AppSetting,MonadUnliftIO)

data User = User {
    user_id    :: Int
  , user_email :: String
  , password :: String
  , userName :: Text
  , isVerified :: Bool
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

getConn :: DBSetting -> ConnectInfo
getConn DBSetting{..} = defaultConnectInfo { connectHost = host,connectDatabase = databaseName,connectUser = user,connectPassword= dbPassword }

instance ToField (PasswordHash Bcrypt) where
  toField = Escape . ST.encodeUtf8 . unPasswordHash

data From = From { email :: Text } deriving (Show,Generic)

data Email = Email { email :: Text } deriving (Show,Generic)

-- https://pastebin.com/raw/RH9bndmx
data MyData = MyData
  { from :: From
  , to :: [Email]
  , subject :: Text
  , text :: Text
  } deriving (Show, Generic)

instance ToJSON MyData
instance ToJSON Email
instance ToJSON From

