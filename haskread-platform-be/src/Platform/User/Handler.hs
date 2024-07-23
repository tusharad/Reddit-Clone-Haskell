{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Platform.User.Handler (registerUserH) where

import Platform.Common.AppM
import Control.Monad.IO.Class
import Platform.User.Types
import Platform.User.DB
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust)
import Data.Char
import UnliftIO
import qualified Data.ByteString.Lazy.Char8 as BS
import Servant
import Platform.DB.Model

toUserWrite :: RegisterUserBody -> UserWrite
toUserWrite RegisterUserBody{..} = User {
    userID = ()
  , userName = userName
  , email = email
  , password = password
  , createdAt = ()
  , updatedAt = ()
}

throw400Err :: MonadIO m => BS.ByteString -> AppM m a
throw400Err err = throwError $ err400 { errBody = err }

doesEmailExists :: MonadUnliftIO m => Text -> AppM m Bool
doesEmailExists email0 = do
    (eRes0 :: Either SomeException (Maybe a)) <- try $ fetchUserByEmailQ email0
    case eRes0 of
        Left e -> throw400Err $ BS.pack $ show e
        Right r -> return $ isJust r

doesUserNameExists :: MonadUnliftIO m => Text -> AppM m Bool
doesUserNameExists userName0 = do
    (eRes0 :: Either SomeException (Maybe a)) <- try $ fetchUserByUserNameQ userName0
    case eRes0 of
        Left e -> throw400Err $ BS.pack $ show e
        Right r -> return $ isJust r

-- Constraints:
-- at least 1 uppercase, 1 lowercase, 1 digit
-- length at lease 8 chars
-- 1 special character
checkPasswordConstraints :: Text -> Bool
checkPasswordConstraints password0 = do
    let res0 = T.length password0 >= 8
        res1 = all (==True) $ [isDigit,isUpper,isLower] <*> (T.unpack password0)
    res0 && res1

getUserID :: UserRead -> UserID
getUserID User{..} = userID

registerUserH :: MonadUnliftIO m => RegisterUserBody -> AppM m RegisterUserResponse
registerUserH userBody@RegisterUserBody{..} = do
    res0 <- doesEmailExists email
    when res0 (throw400Err "email already exists")
    res1 <- doesUserNameExists userName
    when res1 $ throw400Err "UserName already exists :("
    when (password /= confirmPassword) $ throw400Err "Password and confirm Password do not match"
    when (checkPasswordConstraints password) $ throw400Err "Password must have upper,lower chars"
    userRead0 <- addUser (toUserWrite userBody)
    return RegisterUserResponse {
         registerUserResponseMessage = "User registered successfully"
      ,  userID = getUserID userRead0
    }
  where
    addUser userWrite0 = do
        (eRes :: Either SomeException UserRead) <- try $ addUserQ userWrite0
        case eRes of
            Left e -> throw400Err $ BS.pack $ show e
            Right r -> pure r
