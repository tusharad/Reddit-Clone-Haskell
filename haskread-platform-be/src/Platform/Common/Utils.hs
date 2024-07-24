{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Platform.Common.Utils
  ( toUserInfo,
    throw400Err,
    throw401Err,
    passwordUpdatedUser,
    validatePassword,
    passwordConstraintMessage
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.DB.Model
import Data.Text (Text)
import Servant (err400, err401, errBody, throwError)
import Data.Char
import qualified Data.Text as T
import Data.String.Interpolate

toUserInfo :: UserRead -> UserInfo
toUserInfo User {..} = UserInfo userID userName

throw400Err :: (MonadIO m) => BSL.ByteString -> AppM m a
throw400Err err = throwError $ err400 {errBody = err}

throw401Err :: (MonadIO m) => BSL.ByteString -> AppM m a
throw401Err err = throwError $ err401 {errBody = err}

passwordUpdatedUser :: UserRead -> Text -> UserWrite
passwordUpdatedUser u password0 = u {
  password = password0
  , createdAt = ()
  , updatedAt = ()
  , userID = ()
}

-- Constraints:
-- at least 1 uppercase, 1 lowercase, 1 digit
-- length at lease 8 chars
passwordConstraintMessage :: BSL.ByteString
passwordConstraintMessage = [i|
  Password must be at least 8 characters long and contain at least one uppercase letter, 
  one lowercase letter, one number
  |]

validatePassword :: Text -> Bool
validatePassword password0 = do
  let res0 = T.length password0 >= 8
      res1 = T.any isUpper password0 && T.any isLower password0 && T.any isDigit password0
  res0 && res1