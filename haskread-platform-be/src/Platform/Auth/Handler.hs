{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Auth.Handler
  ( registerUserH,
    loginUserH,
    adminLoginH,
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Platform.Admin.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Platform.User.DB
import Platform.User.Types
import Servant
import Servant.Auth.Server
import UnliftIO
import Platform.Admin.DB

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
  (eRes0 :: Either SomeException (Maybe a)) <-
    try $ fetchUserByEmailQ email0
  case eRes0 of
    Left e -> throw400Err $ BSL.pack $ show e
    Right r -> return $ isJust r

doesUserNameExists :: (MonadUnliftIO m) => Text -> AppM m Bool
doesUserNameExists userName0 = do
  (eRes0 :: Either SomeException (Maybe a)) <-
    try $ fetchUserByUserNameQ userName0
  case eRes0 of
    Left e -> throw400Err $ BSL.pack $ show e
    Right r -> return $ isJust r

getUserID :: UserRead -> UserID
getUserID User {..} = userID

registerUserH ::
  (MonadUnliftIO m) =>
  RegisterUserBody ->
  AppM m RegisterUserResponse
registerUserH userBody@RegisterUserBody {..} = do
  res0 <- doesEmailExists email
  when res0 (throw400Err "email already exists")
  res1 <- doesUserNameExists userName
  when res1 $ throw400Err "UserName already exists :("
  when (password /= confirmPassword) $
    throw400Err "Password and confirm Password do not match"
  unless (validatePassword password) $
    throw400Err "Password must have upper,lower chars"
  userRead0 <- addUser (toUserWrite userBody)
  return
    RegisterUserResponse
      { registerUserResponseMessage = "User registered successfully",
        userIDForRUR = getUserID userRead0
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
            Right v ->
              return $
                x
                  ( LoginUserResponse
                      (T.decodeUtf8 $ BSL.toStrict v)
                      "User loggedIn successfully"
                  )
  where
    findUserByMailAndPassword = do
      (eMUser :: Either SomeException (Maybe UserRead)) <-
        try $ findUserByMailPasswordQ email password
      case eMUser of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r

adminLoginH ::
  (MonadUnliftIO m) =>
  CookieSettings ->
  JWTSettings ->
  AdminLoginBodyReq ->
  AppM
    m
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header "Set-Cookie" SetCookie
         ]
        AdminLoginResponse
    )
adminLoginH cookieSett jwtSett AdminLoginBodyReq {..} = do
  mRes <- findAdminByMailAndPassword
  case mRes of
    Nothing -> throw400Err "Email/Password is incorrect"
    Just adminRead0 -> do
      let adminInfo = toAdminInfo adminRead0
      mLoginAccepted <- liftIO $ acceptLogin cookieSett jwtSett adminInfo
      case mLoginAccepted of
        Nothing -> throw401Err "Login failed"
        Just loginAcceptedResp -> do
          etoken <- liftIO $ makeJWT adminInfo jwtSett Nothing
          case etoken of
            Left _ -> throw401Err "JWT token creation failed"
            Right v ->
              return $
                loginAcceptedResp
                  ( AdminLoginResponse
                      (T.decodeUtf8 $ BSL.toStrict v)
                      "Admin loggedIn successfully"
                  )
  where
    findAdminByMailAndPassword = do
      (eMAdmin :: Either SomeException (Maybe AdminRead)) <-
        try $ findAdminByMailPasswordQ adminEmail adminPassword
      case eMAdmin of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r
