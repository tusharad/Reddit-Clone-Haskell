{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.Log
  ( logDebug
  , logError
  , logInfo
  , logWarn
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Platform.Common.AppM
import Platform.Common.Types
import System.Log.FastLogger

logDebug :: (MonadIO m) => Text -> AppM m ()
logDebug msg = do
  res <- asks appConfig
  liftIO $ logger (loggerSet res) (minLogLevel res) LevelDebug msg

logError :: (MonadIO m) => Text -> AppM m ()
logError msg = do
  res <- asks appConfig
  liftIO $ logger (loggerSet res) (minLogLevel res) LevelError msg

logInfo :: (MonadIO m) => Text -> AppM m ()
logInfo msg = do
  res <- asks appConfig
  liftIO $ logger (loggerSet res) (minLogLevel res) LevelInfo msg

logWarn :: (MonadIO m) => Text -> AppM m ()
logWarn msg = do
  res <- asks appConfig
  liftIO $ logger (loggerSet res) (minLogLevel res) LevelWarn msg

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
