{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Platform.Common.Types
  ( MyAppState (..),
    AppConfig (..),
    LogLevel (..),
    MinLogLevel,
    Env (..),
    DBConfig (..),
    MyPassword (..),
  )
where

import Data.Aeson (ToJSON, toJSON)
import Data.Password.Bcrypt
import Dhall
import qualified Haxl.Core as Haxl
import qualified Orville.PostgreSQL as O
import System.Log.FastLogger

data DBConfig = DBConfig
  { host :: Text,
    port :: Natural,
    dbName :: Text,
    dbUserName :: Text,
    dbPassword :: Text
  }
  deriving (Generic, FromDhall, Show)

data Env = Env
  { dbConfig :: DBConfig,
    logFilePath :: FilePath,
    logLevel :: Text,
    fileUploadPath :: FilePath,
    applicationPort :: Natural
  }
  deriving (Generic, FromDhall, Show)

data AppConfig = AppConfig
  { fileUploadDir :: FilePath,
    loggerSet :: LoggerSet,
    minLogLevel :: MinLogLevel
  }

data MyAppState = MyAppState
  { appConfig :: AppConfig,
    appOrvilleState :: O.OrvilleState,
    haxlState :: Haxl.StateStore
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
