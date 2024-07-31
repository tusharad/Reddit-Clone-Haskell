{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Platform.Log (
    logDebug
) where

import System.Log.FastLogger
import Data.Time
import Platform.Common.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Platform.Common.AppM

logDebug :: (MonadIO m) => Text -> AppM m ()
logDebug msg = do
    res <- asks appConfig
    liftIO $ logger (loggerSet res) (minLogLevel res) LevelDebug msg

logger :: ToLogStr msg => LoggerSet -> MinLogLevel -> LogLevel -> msg -> IO ()
logger loggerSet_ minLogLevel_ logLevel0 msg = do
    when (logLevel0 >= minLogLevel_) $ do
        currTime <- getCurrentTime
        let logMsg = [
                  show logLevel0
                , show currTime
                , ": "
                , BS.unpack $ (fromLogStr.toLogStr) msg]
        pushLogStrLn loggerSet_ $ toLogStr $ (T.pack.unwords) logMsg


