{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.Common.Types
  ( MyAppState (..)
  , AppConfig (..)
  , LogLevel (..)
  , MinLogLevel
  , Env (..)
  , DBConfig (..)
  , MyPassword (..)
  , HaxlConfig (..)
  , OAuth2Config (..)
  , RandomUsername (..)
  , RandomUserNameApiResponse (..)
  , Environment (..)
  )
where

import Control.Concurrent.QSem
import Data.Aeson -- (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Password.Bcrypt
import Dhall

-- import qualified Haxl.Core as Haxl
import qualified Orville.PostgreSQL as O
import Servant.Auth.Server (CookieSettings, JWTSettings)
import System.Log.FastLogger

data DBConfig = DBConfig
  { host :: Text
  , port :: Natural
  , dbName :: Text
  , dbUserName :: Text
  , dbPassword :: Text
  }
  deriving (Generic, FromDhall, Show)

data Environment = Production | Development | Sandbox | Test | Local
  deriving (Generic, Eq, Ord, Show, ToJSON, FromDhall)

data OAuth2Config = OAuth2Config
  { clientID :: Text
  , clientSecret :: Text
  }
  deriving (Generic, FromDhall, Show)

data Env = Env
  { dbConfig :: DBConfig
  , logFilePath :: FilePath
  , logLevel :: Text
  , fileUploadPath :: FilePath
  , applicationPort :: Natural
  , mailAPIToken :: Text
  , mailFromEmail :: Text
  , oauth2Config :: OAuth2Config
  , tokenExpiryTime :: Natural
  , environment_ :: Text
  , ip_ :: Text
  }
  deriving (Generic, FromDhall, Show)

data AppConfig = AppConfig
  { fileUploadDir :: FilePath
  , loggerSet :: LoggerSet
  , minLogLevel :: MinLogLevel
  , emailAPIToken :: Text
  , emailFromEmail :: Text
  , jwtSett :: JWTSettings
  , cookieSett :: CookieSettings
  , googleOauth2Config :: OAuth2Config
  , tokenExpiryTime0 :: Integer
  , environment :: Environment
  , ip :: Text
  }

data HaxlConfig = HaxlConfig
  { pgConnectionPool :: O.ConnectionPool
  , numOfThreads :: QSem
  }

data MyAppState = MyAppState
  { appConfig :: AppConfig
  , appOrvilleState :: O.OrvilleState
  , haxlConfig :: HaxlConfig
  }

-- Log types
data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Eq, Ord, Generic, FromDhall)

instance Show LogLevel where
  show LevelDebug = "[Debug]"
  show LevelInfo = "[Info]"
  show LevelWarn = "[Warn]"
  show LevelError = "[Error]"

type MinLogLevel = LogLevel

-- End of log types

newtype MyPassword = MyPassword
  {getPassword :: (PasswordHash Bcrypt)}
  deriving newtype (Show, Eq)

instance ToJSON MyPassword where
  toJSON myPassword = toJSON (unPasswordHash (getPassword myPassword))

newtype RandomUsername = RandomUsername
  { randomUsername :: String
  }
  deriving (Show, Generic)

instance FromJSON RandomUsername where
  parseJSON = withObject "Username" $ \v -> do
    login <- v .: "login"
    RandomUsername <$> login .: "username"

newtype RandomUserNameApiResponse = RandomUserNameApiResponse
  { results :: [RandomUsername]
  }
  deriving (Show, Generic)

instance FromJSON RandomUserNameApiResponse
