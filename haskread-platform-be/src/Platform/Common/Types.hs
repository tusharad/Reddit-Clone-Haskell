module Platform.Common.Types (
    MyAppState(..)
 ,  AppConfig(..)
 , LogLevel(..)
  , MinLogLevel
) where

import qualified Orville.PostgreSQL as O
import System.Log.FastLogger

data AppConfig = AppConfig {
    fileUploadDir :: String
  , loggerSet :: LoggerSet
  , minLogLevel :: MinLogLevel
}
data MyAppState = MyAppState {
    appConfig :: AppConfig
  , appOrvilleState :: O.OrvilleState
} 

-- Log types
data LogLevel = LevelDebug
              | LevelInfo
              | LevelWarn
              | LevelError
  deriving (Eq,Ord)

instance Show LogLevel where
  show LevelDebug = "[Debug]"
  show LevelInfo = "[Info]"
  show LevelWarn = "[Warn]"
  show LevelError = "[Error]"

type MinLogLevel = LogLevel
-- End of log types