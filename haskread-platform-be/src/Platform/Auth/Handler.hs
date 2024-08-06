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
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (isJust)
import Data.Password.Bcrypt
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Haxl.Core (dataFetch, initEnv, runHaxl, stateEmpty, stateSet)
import qualified Haxl.Core as Haxl
-- import Platform.Admin.DB (fetchAdminByEmailQ)
import Platform.Admin.Types
import Platform.Common.AppM
import Platform.Common.Types
import Platform.Common.Utils
import Platform.DB.Model
import Platform.Haxl.DataSource
import Platform.User.DB
import Platform.User.Types
import Servant
import Servant.Auth.Server
import UnliftIO

toUserWrite :: RegisterUserBody -> IO UserWrite
toUserWrite RegisterUserBody {..} = do
  hashedPass <- MyPassword <$> hashPassword (mkPassword passwordForRegister)
  pure $
    User
      { userID = (),
        userName = userNameForRegister,
        email = emailForRegister,
        userPassword = hashedPass,
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
  res0 <- doesEmailExists emailForRegister
  when res0 (throw400Err "email already exists")
  res1 <- doesUserNameExists userNameForRegister
  when res1 $ throw400Err "UserName already exists :("
  when (passwordForRegister /= confirmPasswordForRegister) $
    throw400Err "Password and confirm Password do not match"
  unless (validatePassword passwordForRegister) $
    throw400Err "Password must have upper,lower chars"
  userWrite0 <- liftIO $ toUserWrite userBody
  userRead0 <- addUser userWrite0
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
  mRes <- findUserByMail
  case mRes of
    Nothing -> throw400Err "Email/Password is incorrect"
    Just userRead0 -> do
      if not $ matchPasswords (userPassword userRead0) passwordForLogin
        then throw400Err "Email/Password is incorrect"
        else do
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
    findUserByMail = do
      (eMUser :: Either SomeException (Maybe UserRead)) <-
        try $ fetchUserByEmailQ emailForLogin
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
  mRes <- findAdminByEmail
  case mRes of
    Nothing -> throw400Err "Email/Password is incorrect"
    Just adminRead0 -> do
      if not $ matchPasswords (adminPassword adminRead0) adminPasswordForLogin
        then throw400Err "Email/Password is incorrect"
        else do
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
    findAdminByEmail :: (MonadUnliftIO m) => AppM m (Maybe AdminRead)
    findAdminByEmail = do
      MyAppState {pgConnectionPool = pool, numOfThreads = sem} <- ask
      let st = HaskReadState pool sem
      eMAdmin :: Either SomeException (Maybe AdminRead) <-
        liftIO $ do
          env0 <- initEnv (stateSet st stateEmpty) () :: IO (Haxl.Env () [Int])
          try $
            runHaxl env0 (dataFetch (GetAdminByEmail adminEmailForLogin))
      case eMAdmin of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r

{-
 findAdminByEmail = do
   (eMAdmin :: Either SomeException (Maybe AdminRead)) <-
     try $ fetchAdminByEmailQ adminEmailForLogin
   case eMAdmin of
     Left e -> throw400Err $ BSL.pack $ show e
     Right r -> pure r
 -}
