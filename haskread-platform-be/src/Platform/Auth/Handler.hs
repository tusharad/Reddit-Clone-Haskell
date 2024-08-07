{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Auth.Handler
  ( registerUserH,
    loginUserH,
    adminLoginH,
    verifyEmailH,
    resendVerifyEmailH,
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Password.Bcrypt
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Int (Int32)
import Haxl.Core (dataFetch, initEnv, runHaxl, stateEmpty, stateSet)
import qualified Haxl.Core as Haxl
import Platform.Admin.Types
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Types
import Platform.Common.Utils
import Platform.DB.Model
import Platform.Email
import Platform.Haxl.DataSource
import Platform.User.DB
import Platform.User.Types
import Servant
import Servant.Auth.Server
import System.Random
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
        isUserVerified = False,
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

addUEVO :: (MonadUnliftIO m) => UserEmailVerifyOTPWrite -> AppM m ()
addUEVO uevo@UserEmailVerifyOTP {userIDForUEVO = uID} = do
  eRes0 :: (Either SomeException (Maybe UserEmailVerifyOTPRead)) <-
    try $
      fetchUEVOByIDQ uID
  case eRes0 of
    Left e -> throw400Err $ BSL.pack $ show e
    Right Nothing -> do
      eRes1 :: Either SomeException () <- try $ addUEVOQ uevo
      case eRes1 of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> pure ()
    Right (Just _) -> do
      eRes2 :: Either SomeException () <- try $ deleteUEVOQ uID
      case eRes2 of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> do
          eRes3 :: Either SomeException () <- try $ addUEVOQ uevo
          case eRes3 of
            Left e -> throw400Err $ BSL.pack $ show e
            Right _ -> pure ()

sendOTPForEmailVerify :: (MonadUnliftIO m) => UserID -> Text -> AppM m ()
sendOTPForEmailVerify userID0 userEmail0 = do
  -- generate 4 digit OTP
  otp <- liftIO $ randomRIO (1000, 9999)
  addUEVO
    UserEmailVerifyOTP
      { userIDForUEVO = userID0,
        otpForUEVO = otp,
        createdAtForUEVO = ()
      }
  AppConfig {emailAPIToken = apiToken, emailFromEmail = fromEmail} <-
    asks appConfig
  eRes <- liftIO $ sendVerificationEmail userEmail0 otp apiToken fromEmail
  case eRes of
    Left e -> do
      -- upon sending verify email failure, the user record shall be
      -- deleted from the database
      eRes1 :: Either SomeException () <- try $ deleteUserQ userID0
      case eRes1 of
        Left err -> throw400Err $ e <> (BSL.pack $ show err)
        Right _ -> throw400Err e
    Right _ -> pure ()

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
  void $ sendOTPForEmailVerify (getUserID userRead0) (email userRead0)
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
          -- If the user is not verified, throw error
          when (not $ isUserVerified userRead0) (throw400Err "User is not verified")
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
      MyAppState
        { haxlConfig =
            HaxlConfig
              { pgConnectionPool = pool,
                numOfThreads = sem
              }
        } <-
        ask
      let st = HaskReadState pool sem
      eMAdmin :: Either SomeException (Maybe AdminRead) <-
        liftIO $ do
          env0 <- initEnv (stateSet st stateEmpty) () :: IO (Haxl.Env () [Int])
          try $
            runHaxl env0 (dataFetch (GetAdminByEmail adminEmailForLogin))
      case eMAdmin of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r

verifyEmailH :: (MonadUnliftIO m) => UserID -> Int32 -> AppM m VerifyEmailResponse
verifyEmailH userID0 otp0 = do
  mUserOTP <- fetchUEVOByID userID0
  case mUserOTP of
    Nothing -> throw400Err "OTP record not found. Please resend verify request"
    Just UserEmailVerifyOTP {..} -> do
      when (otpForUEVO /= otp0) (throw400Err "Incorrect OTP!")
      deleteUEVO userID0
      mkUserVerified
      pure $ VerifyEmailResponse "User has been successfully verified!"
  where
    mkUserVerified = do
      mUser <- fetchUserByID userID0
      case mUser of
        Nothing -> throw400Err "Something went wrong!"
        Just u -> do
          let userWrite0 =
                u
                  { isUserVerified = True, -- Main login
                    userID = (),
                    createdAt = (),
                    updatedAt = ()
                  }
          updateUser userID0 userWrite0

resendVerifyEmailH :: (MonadUnliftIO m) => UserID -> AppM m ResendVerifyEmailResponse
resendVerifyEmailH userID0 = do
  mUserOTP <- fetchUEVOByID userID0
  when (isNothing mUserOTP) $ deleteUEVO userID0
  mUser <- fetchUserByID userID0
  when (isNothing mUser) $ throw400Err "User does not exists!" -- impossible case
  void $ sendOTPForEmailVerify userID0 (fromMaybe "" (email <$> mUser))
  pure $ ResendVerifyEmailResponse "Verification mail has been sent!"
