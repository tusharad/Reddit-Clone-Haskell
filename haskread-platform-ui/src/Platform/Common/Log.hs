{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Platform.Common.Log
  ( logDebug
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Effectful
import Effectful.Reader.Dynamic
import Platform.Common.Types
import System.Log.FastLogger

logDebug :: (Reader AppConfig :> es, IOE :> es) => Text -> Eff es ()
logDebug msg = do
  res <- ask
  liftIO $ logger (loggerSet res) (minLogLevel res) LevelDebug msg

logger :: ToLogStr msg => LoggerSet -> MinLogLevel -> LogLevel -> msg -> IO ()
logger loggerSet_ minLogLevel_ logLevel0 msg = do
  when (logLevel0 >= minLogLevel_) $ do
    currTime <- getCurrentTime
    let logMsg =
          [ show logLevel0
          , show currTime
          , ": "
          , BS.unpack $ (fromLogStr . toLogStr) msg
          ]
    pushLogStrLn loggerSet_ $ toLogStr $ (T.pack . unwords) logMsg
