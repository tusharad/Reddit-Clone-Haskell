{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Auth.Handler
  ( registerUserH,
    loginUserH,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Platform.User.DB
import Platform.User.Types
import Servant
import Servant.Auth.Server
import UnliftIO

toUserWrite :: RegisterUserBody -> UserWrite
toUserWrite RegisterUserBody {..} =
  User
    { userID = (),
      userName = userName,
      email = email,
      password = password,
      createdAt = (),
      updatedAt = ()
    }

doesEmailExists :: (MonadUnliftIO m) => Text -> AppM m Bool
doesEmailExists email0 = do
  (eRes0 :: Either SomeException (Maybe a)) <- try $ fetchUserByEmailQ email0
  case eRes0 of
    Left e -> throw400Err $ BSL.pack $ show e
    Right r -> return $ isJust r

doesUserNameExists :: (MonadUnliftIO m) => Text -> AppM m Bool
doesUserNameExists userName0 = do
  (eRes0 :: Either SomeException (Maybe a)) <- try $ fetchUserByUserNameQ userName0
  case eRes0 of
    Left e -> throw400Err $ BSL.pack $ show e
    Right r -> return $ isJust r

-- Constraints:
-- at least 1 uppercase, 1 lowercase, 1 digit
-- length at lease 8 chars
-- 1 special character
checkPasswordConstraints :: Text -> Bool
checkPasswordConstraints password0 = do
  let res0 = T.length password0 >= 8
      res1 = all (== True) $ [isDigit, isUpper, isLower] <*> (T.unpack password0)
  res0 && res1

getUserID :: UserRead -> UserID
getUserID User {..} = userID

registerUserH :: (MonadUnliftIO m) => RegisterUserBody -> AppM m RegisterUserResponse
registerUserH userBody@RegisterUserBody {..} = do
  res0 <- doesEmailExists email
  when res0 (throw400Err "email already exists")
  res1 <- doesUserNameExists userName
  when res1 $ throw400Err "UserName already exists :("
  when (password /= confirmPassword) $ throw400Err "Password and confirm Password do not match"
  when (checkPasswordConstraints password) $ throw400Err "Password must have upper,lower chars"
  userRead0 <- addUser (toUserWrite userBody)
  return
    RegisterUserResponse
      { registerUserResponseMessage = "User registered successfully",
        userID = getUserID userRead0
      }
  where
    addUser userWrite0 = do
      (eRes :: Either SomeException UserRead) <- try $ addUserQ userWrite0
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r

loginUserH ::
  (MonadUnliftIO m) =>
  CookieSettings ->
  JWTSettings ->
  LoginUserBody ->
  AppM
    m
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header "Set-Cookie" SetCookie
         ]
        LoginUserResponse
    )
loginUserH cookieSett jwtSett LoginUserBody {..} = do
  mRes <- findUserByMailAndPassword
  case mRes of
    Nothing -> throw400Err "Email/Password is incorrect"
    Just userRead0 -> do
      -- do login
      let userInfo = toUserInfo userRead0
      mLoginAccepted <- liftIO $ acceptLogin cookieSett jwtSett userInfo
      case mLoginAccepted of
        Nothing -> throwError err401
        Just x -> do
          etoken <- liftIO $ makeJWT userInfo jwtSett Nothing
          case etoken of
            Left _ -> throwError err401 {errBody = "JWT token creation failed"}
            Right v -> return $ x (LoginUserResponse (T.decodeUtf8 $ BSL.toStrict v) "User loggedIn successfully")
  where
    findUserByMailAndPassword = do
      (eMUser :: Either SomeException (Maybe UserRead)) <-
        try $ findUserByMailPasswordQ email password
      case eMUser of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r
