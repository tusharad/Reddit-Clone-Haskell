module Platform.Common.Types (
    MyAppState(..)
 ,  AppConfig(..)
) where

import qualified Orville.PostgreSQL as O

data AppConfig = AppConfig {
    fileUploadDir :: String
}

data MyAppState = MyAppState {
    appConfig :: AppConfig
  , appOrvilleState :: O.OrvilleState
}