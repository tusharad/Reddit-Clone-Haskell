{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Auth.Handler
  ( registerUserH
  , loginUserH
  , adminLoginH
  , verifyEmailH
  , resendVerifyEmailH
  , oauth2LoginH
  , oauth2CallbackH
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Password.Bcrypt
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Time
import GHC.Int (Int32)
import Haxl.Core (dataFetch, initEnv, runHaxl, stateEmpty, stateSet)
import qualified Haxl.Core as Haxl
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.OAuth.OAuth2
  ( ExchangeToken (ExchangeToken)
  , OAuth2Token (accessToken)
  )
import Network.OAuth2.Experiment
import Network.OAuth2.Provider (IdpName (Google))
import qualified Network.OAuth2.Provider.Google as Google
import Platform.Admin.Types
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Types
import Platform.Common.Utils
import Platform.DB.Model
import Platform.Email
import Platform.Haxl.DataSource
import Platform.Log
import Platform.User.DB
import Platform.User.Types
import Servant
import Servant.Auth.Server
import System.Random
import URI.ByteString (parseURI, strictURIParserOptions)
import URI.ByteString.QQ (uri)
import UnliftIO

toUserWrite :: RegisterUserBody -> IO UserWrite
toUserWrite RegisterUserBody {..} = do
  hashedPass <- MyPassword <$> hashPassword (mkPassword passwordForRegister)
  pure $
    User
      { userID = ()
      , userName = userNameForRegister
      , email = emailForRegister
      , userPassword = Just hashedPass
      , isUserVerified = False
      , createdAt = ()
      , updatedAt = ()
      }

doesEmailExists :: (MonadUnliftIO m) => Text -> AppM m Bool
doesEmailExists email0 = do
  eRes0 :: Either SomeException (Maybe a) <- try $ fetchUserByEmailQ email0
  case eRes0 of
    Left e -> do
      logError $ "DB Exception: doesEmailExists: " <> email0 <> toText e
      throw400Err $ BSL.pack $ show e
    Right r -> return $ isJust r

doesUserNameExists :: (MonadUnliftIO m) => Text -> AppM m Bool
doesUserNameExists userName0 = do
  (eRes0 :: Either SomeException (Maybe a)) <-
    try $ fetchUserByUserNameQ userName0
  case eRes0 of
    Left e -> do
      logError $ "DB Exception: doesUserNameExists: " <> userName0 <> toText e
      throw400Err $ BSL.pack $ show e
    Right r -> return $ isJust r

getUserID :: UserRead -> UserID
getUserID User {..} = userID

addUEVO :: (MonadUnliftIO m) => UserEmailVerifyOTPWrite -> AppM m ()
addUEVO uevo@UserEmailVerifyOTP {userIDForUEVO = uID} = do
  eRes0 :: (Either SomeException (Maybe UserEmailVerifyOTPRead)) <-
    try $
      fetchUEVOByIDQ uID
  case eRes0 of
    Left e -> do 
      logError $ "DB Exception: fetchUEVOByIDQ " <> toText uID <> toText e
      throw400Err $ BSL.pack $ show e
    Right Nothing -> do
      eRes1 :: Either SomeException () <- try $ addUEVOQ uevo
      case eRes1 of
        Left e -> do 
          logError $ "DB Exception: addUEVOQ " <> toText uID <> toText e
          throw400Err $ BSL.pack $ show e
        Right _ -> logDebug $ "OTP Added for userId " <> toText uID
    Right (Just _) -> do
      eRes2 :: Either SomeException () <- try $ deleteUEVOQ uID
      case eRes2 of
        Left e -> do 
          logError $ "DB Exception: deleteUEVOQ " <> toText uID <> toText e
          throw400Err $ BSL.pack $ show e
        Right _ -> do
          eRes3 :: Either SomeException () <- try $ addUEVOQ uevo
          case eRes3 of
            Left e -> throw400Err $ BSL.pack $ show e
            Right _ -> logDebug $ "OTP Updated for userId" <> toText uID

sendOTPForEmailVerify :: (MonadUnliftIO m) => UserID -> Text -> AppM m ()
sendOTPForEmailVerify userID0 userEmail0 = do
  -- generate 4 digit OTP
  otp <- liftIO $ randomRIO (1000, 9999)
  addUEVO
    UserEmailVerifyOTP
      { userIDForUEVO = userID0
      , otpForUEVO = otp
      , createdAtForUEVO = ()
      }
  AppConfig {emailAPIToken = apiToken} <-
    asks appConfig
  eRes <- liftIO $ sendVerificationEmail apiToken userEmail0 otp
  case eRes of
    Left e -> do
      -- upon sending verify email failure, the user record shall be
      -- deleted from the database
      eRes1 :: Either SomeException () <- try $ deleteUserQ userID0
      case eRes1 of
        Left err -> throw400Err $ e <> (BSL.pack $ show err)
        Right _ -> do 
          logDebug $ "Sending otp failed, deleting registered usered: " <> userEmail0
          throw400Err e
    Right _ -> logDebug $ "OTP email sent successfully :" <> userEmail0

registerUserH ::
  (MonadUnliftIO m) =>
  RegisterUserBody ->
  AppM m RegisterUserResponse
registerUserH userBody@RegisterUserBody {..} = do
  res0 <- doesEmailExists emailForRegister
  when res0 $ do
    logDebug $ "email already exists: " <> emailForRegister
    throw400Err "email already exists"
  res1 <- doesUserNameExists userNameForRegister
  when res1 $ throw400Err "UserName already exists :("
  when (passwordForRegister /= confirmPasswordForRegister) $
    throw400Err "Password and confirm Password do not match"
  unless (validatePassword passwordForRegister) $
    throw400Err "Password must have upper,lower chars"
  userWrite0 <- liftIO $ toUserWrite userBody
  userRead0 <- addUser userWrite0
  void $ sendOTPForEmailVerify (getUserID userRead0) (email userRead0)
  logDebug $ "user inserted successfully: " <> toText userRead0
  return
    RegisterUserResponse
      { registerUserResponseMessage = "User registered successfully"
      , userIDForRUR = getUserID userRead0
      }

loginUserH ::
  (MonadUnliftIO m) =>
  LoginUserBody ->
  AppM
    m
    ( Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
        LoginUserResponse
    )
loginUserH LoginUserBody {..} = do
  mRes <- findUserByMail
  case mRes of
    Nothing -> throw400Err "Email/Password is incorrect"
    Just userRead0 -> do
      logDebug $ "login process initiated for user: " <> toText userRead0
      let mUPassword = userPassword userRead0
      case mUPassword of
        Nothing -> throw400Err "Email/Password is incorrect"
        Just uPassword -> do
          if not $
            matchPasswords uPassword passwordForLogin
            then throw400Err "Email/Password is incorrect"
            else do
              -- If the user is not verified, throw error
              unless (isUserVerified userRead0) (throw400Err "User is not verified")
              -- do login
              loginUser userRead0 False
  where
    findUserByMail = do
      (eMUser :: Either SomeException (Maybe UserRead)) <-
        try $ fetchUserByEmailQ emailForLogin
      case eMUser of
        Left e -> throw400Err $ BSL.pack $ show e
        Right r -> pure r

adminLoginH ::
  (MonadUnliftIO m) =>
  AdminLoginBodyReq ->
  AppM
    m
    ( Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
        AdminLoginResponse
    )
adminLoginH AdminLoginBodyReq {..} = do
  AppConfig {..} <- asks appConfig
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
              now <- liftIO getCurrentTime
              etoken <-
                liftIO $
                  makeJWT adminInfo jwtSett $
                    Just $
                      (fromInteger tokenExpiryTime0) `addUTCTime` now
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
            { pgConnectionPool = pool
            , numOfThreads = sem
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
                  { isUserVerified = True -- Main login
                  , userID = ()
                  , createdAt = ()
                  , updatedAt = ()
                  }
          updateUser userID0 userWrite0

resendVerifyEmailH :: (MonadUnliftIO m) => UserID -> AppM m ResendVerifyEmailResponse
resendVerifyEmailH userID0 = do
  mUserOTP <- fetchUEVOByID userID0
  when (isNothing mUserOTP) $ deleteUEVO userID0
  mUser <- fetchUserByID userID0
  when (isNothing mUser) $ throw400Err "User does not exists!" -- impossible case
  void $ sendOTPForEmailVerify userID0 (maybe "" email mUser)
  pure $ ResendVerifyEmailResponse "Verification mail has been sent!"

oauth2LoginH :: (MonadUnliftIO m) => AuthResult UserInfo -> AppM m NoContent
oauth2LoginH (Authenticated _) = throw401Err "User alread logged IN"
oauth2LoginH _ = do
  googleApp <- mkTestGoogleApp
  void $ redirects (T.encodeUtf8 $ uriToText $ mkAuthorizationRequest googleApp)
  pure NoContent

mkTestGoogleApp ::
  (MonadIO m) =>
  AppM m (IdpApplication Google AuthorizationCodeApplication)
mkTestGoogleApp = do
  googleOAuth2Cfg <- asks (googleOauth2Config . appConfig)
  AppConfig {..} <- asks appConfig
  let url =
        if environment == Production
          then
            parseURI strictURIParserOptions ("https://" <> T.encodeUtf8 ip <> "/callback")
          else
            parseURI strictURIParserOptions "http://localhost:8085/callback"
  let application =
        AuthorizationCodeApplication
          { acClientId = ClientId $ TL.fromStrict (clientID googleOAuth2Cfg)
          , acClientSecret = ClientSecret $ TL.fromStrict (clientSecret googleOAuth2Cfg)
          , acAuthorizeState = AuthorizeState ("google." <> randomStateValue)
          , acRedirectUri = fromRight [uri|http://localhost:8085/callback|] url
          , acScope =
              Set.fromList
                [ "https://www.googleapis.com/auth/userinfo.email"
                , "https://www.googleapis.com/auth/userinfo.profile"
                ]
          , acName = "haskread-app"
          , acAuthorizeRequestExtraParams = Map.empty
          , acTokenRequestAuthenticationMethod = ClientSecretBasic
          }
      idp = Google.defaultGoogleIdp
  pure IdpApplication {..}

randomStateValue :: TL.Text
randomStateValue = "random-state-to-prevent-csrf"

oauth2CallbackH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  Maybe Text ->
  Maybe Text ->
  AppM
    m
    ( Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
        LoginUserResponse
    )
oauth2CallbackH (Authenticated _) _ _ = throw401Err "User alread logged IN"
oauth2CallbackH _ (Just _) (Just codeP) = do
  liftIO $ print ("getting callback" :: String)
  let code = ExchangeToken codeP
  -- idpName = T.takeWhile ('.' /=) stateP
  googleApp <- mkTestGoogleApp
  mgr <- liftIO $ newManager tlsManagerSettings
  eTokenResp <- runExceptT (conduitTokenRequest googleApp mgr code)
  case eTokenResp of
    Left e -> throw401Err $ BSL.pack $ show e
    Right tokenResp -> do
      eGoogleUser <- runExceptT $ Google.fetchUserInfo googleApp mgr (accessToken tokenResp)
      case eGoogleUser of
        Left err -> throw401Err $ BSL.pack $ show err
        Right gUser -> do
          let gUserEmail = TL.toStrict $ Google.email gUser
          -- find User in DB
          mUser0 <- fetchUserByEmail gUserEmail
          case mUser0 of
            Nothing -> registerAndLoginUser gUserEmail
            Just userRead0 -> loginUser userRead0 True
  where
    registerAndLoginUser ::
      (MonadUnliftIO m) =>
      Text ->
      AppM
        m
        ( Headers
            '[ Header "Set-Cookie" SetCookie
             , Header "Set-Cookie" SetCookie
             ]
            LoginUserResponse
        )
    registerAndLoginUser email0 = do
      randomUserName <- liftIO genRandomUserName
      let randomUserName' = fromRight "not getting value" randomUserName
      let userWrite0 =
            User
              { userID = ()
              , userName = randomUserName'
              , email = email0
              , userPassword = Nothing
              , isUserVerified = True
              , createdAt = ()
              , updatedAt = ()
              }
      userRead0 <- addUser userWrite0
      loginUser userRead0 True
oauth2CallbackH _ _ _ = throw401Err "Oauth failed :("

loginUser ::
  (MonadUnliftIO m) =>
  UserRead ->
  Bool ->
  AppM
    m
    ( Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
        LoginUserResponse
    )
loginUser userRead0 isOAuth = do
  AppConfig {..} <- asks appConfig
  let userInfo = toUserInfo userRead0
  mLoginAccepted <- liftIO $ acceptLogin cookieSett jwtSett userInfo
  case mLoginAccepted of
    Nothing -> throwError err401
    Just x -> do
      now <- liftIO getCurrentTime
      etoken <-
        liftIO $
          makeJWT userInfo jwtSett $
            Just $
              fromInteger tokenExpiryTime0 `addUTCTime` now
      case etoken of
        Left _ -> throwError err401 {errBody = "JWT token creation failed"}
        Right v -> do
          if isOAuth
            then do
              logDebug $ "calling Oouth2/callback " <> toText v 
              --TODO: This logic shall be moved to frontend
              let redirectUrl =
                    if environment == Production
                      then
                        "https://" <> BSL.fromStrict (T.encodeUtf8 ip) <> "/oauth2/callback?token=" <> v
                      else
                        "http://localhost:3000/oauth2/callback?token=" <> v
              logDebug $ "User logged in: " <> toText userInfo
              void $ redirects $ BSL.toStrict redirectUrl
              return $ x (LoginUserResponse (T.decodeUtf8 $ BSL.toStrict v) "")
            else do 
              logDebug $ "User logged in: " <> toText userInfo
              return $
                x
                  ( LoginUserResponse
                      (T.decodeUtf8 $ BSL.toStrict v)
                      "User loggedIn successfully"
                  )
