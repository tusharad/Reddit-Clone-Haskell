{-# LANGUAGE RecordWildCards #-}

module Platform.Common.Utils
  ( toUserInfo,
    throw400Err,
    throw401Err,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.DB.Model
import Servant (err400, err401, errBody, throwError)

toUserInfo :: UserRead -> UserInfo
toUserInfo User {..} = UserInfo userID userName

throw400Err :: (MonadIO m) => BSL.ByteString -> AppM m a
throw400Err err = throwError $ err400 {errBody = err}

throw401Err :: (MonadIO m) => BSL.ByteString -> AppM m a
throw401Err err = throwError $ err401 {errBody = err}
