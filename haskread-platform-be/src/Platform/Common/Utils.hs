{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Platform.Common.Utils
  ( toUserInfo,
    throw400Err,
    throw401Err,
    passwordUpdatedUser,
    validatePassword,
    passwordConstraintMessage,
    toAdminInfo,
    toJsonbArray,
    readEnv,
    matchPasswords,
    queryWrapper,
    redirects,
    genRandomUserName,
    threadToThreadInfo,
  )
where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char
import Data.Password.Bcrypt
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Dhall
import Network.HTTP.Req as Req
import qualified Orville.PostgreSQL as O
import Orville.PostgreSQL.Raw.Connection
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Types
import Platform.DB.Model
import Servant as S
import Servant.Auth.Server
import System.Log.FastLogger
import UnliftIO

toUserInfo :: UserRead -> UserInfo
toUserInfo User {..} = UserInfo userID userName

toAdminInfo :: AdminRead -> AdminInfo
toAdminInfo Admin {..} = AdminInfo adminID adminName

throw400Err :: (MonadIO m) => BSL.ByteString -> AppM m a
throw400Err err = throwError $ err400 {errBody = err}

throw401Err :: (MonadIO m) => BSL.ByteString -> AppM m a
throw401Err err = throwError $ err401 {errBody = err}

passwordUpdatedUser :: UserRead -> PasswordHash Bcrypt -> UserWrite
passwordUpdatedUser u password0 =
  u
    { userPassword = Just (MyPassword password0),
      createdAt = (),
      updatedAt = (),
      userID = ()
    }

-- Constraints:
-- at least 1 uppercase, 1 lowercase, 1 digit
-- length at lease 8 chars
passwordConstraintMessage :: BSL.ByteString
passwordConstraintMessage =
  [i|
  Password must be at least 8 characters long and contain at least one uppercase letter, 
  one lowercase letter, one number
  |]

validatePassword :: Text -> Bool
validatePassword password0 = do
  let res0 = T.length password0 >= 8
      res1 = T.any isUpper password0 && T.any isLower password0 && T.any isDigit password0
  res0 && res1

toJsonbArray :: [Text] -> Text
toJsonbArray = T.pack . show

mkConnection :: DBConfig -> ConnectionOptions
mkConnection DBConfig {..} =
  let connString =
        unwords
          [ "dbname=" <> T.unpack dbName,
            "host=" <> T.unpack host,
            "user=" <> T.unpack dbUserName,
            "password=" <> T.unpack dbPassword,
            "port=" <> show port
          ]
   in ConnectionOptions
        { connectionString = connString,
          connectionNoticeReporting = DisableNoticeReporting,
          connectionPoolStripes = OneStripePerCapability,
          connectionPoolMaxConnections = MaxConnectionsPerStripe 1,
          connectionPoolLingerTime = 10
        }

toLogLevel :: Text -> MinLogLevel
toLogLevel "LevelDebug" = LevelDebug
toLogLevel "LevelInfo" = LevelInfo
toLogLevel "LevelWarn" = LevelWarn
toLogLevel "LevelError" = LevelError
toLogLevel _ = LevelInfo -- Default if wrong value is mentioned

-- This function wraps query function in try and throws
-- 400 Error on exception
queryWrapper :: (MonadUnliftIO m) => AppM m a -> AppM m a
queryWrapper queryFunc = do
  eRes <- try queryFunc
  case eRes of
    Left e -> throw400Err $ BSL.pack (show (e :: SomeException))
    Right r -> pure r

data MissingEnvArgumentException = MissingEnvArgumentException Text
  deriving (Show)

instance Exception MissingEnvArgumentException

matchPasswords :: MyPassword -> Text -> Bool
matchPasswords myPassword p =
  case checkPassword (mkPassword p) (getPassword myPassword) of
    PasswordCheckSuccess -> True
    _ -> False

readEnv ::
  String ->
  IO
    ( Either
        SomeException
        ( MyAppState,
          JWTSettings,
          Context [CookieSettings, JWTSettings],
          Int,
          ConnectionPool
        )
    )
readEnv envFilePath = do
  eEnv <-
    try $ input auto (T.pack $ envFilePath) ::
      IO (Either SomeException Env)
  case eEnv of
    Left e -> pure $ Left e
    Right Env {..} -> do
      putStrLn $ "Environment loaded successfully: " <> envFilePath
      ePool <-
        try (createConnectionPool $ mkConnection dbConfig) ::
          IO (Either SqlExecutionError ConnectionPool)
      case ePool of
        Left e -> pure $ Left $ toException e
        Right pool -> do
          let jwtSecretKey = fromSecret $ T.encodeUtf8 jwtSecretKey_
          loggerSet_ <- newFileLoggerSet defaultBufSize logFilePath
          sem <- newQSem 10 -- 10 threads
          let orvilleState = O.newOrvilleState O.defaultErrorDetailLevel pool
              jwtSett = defaultJWTSettings jwtSecretKey
              appCfg =
                AppConfig
                  { fileUploadDir = fileUploadPath,
                    loggerSet = loggerSet_,
                    minLogLevel = toLogLevel logLevel,
                    emailAPIToken = mailAPIToken,
                    emailFromEmail = mailFromEmail,
                    googleOauth2Config = oauth2Config,
                    jwtSett = jwtSett,
                    cookieSett =
                      defaultCookieSettings
                        { cookieMaxAge =
                            Just $
                              secondsToDiffTime $
                                fromIntegral tokenExpiryTime
                        },
                    tokenExpiryTime0 = fromIntegral tokenExpiryTime
                  }
              appST =
                MyAppState
                  appCfg
                  orvilleState
                  (HaxlConfig pool sem)
              ctx = defaultCookieSettings :. jwtSett :. EmptyContext
          pure $ Right (appST, jwtSett, ctx, fromIntegral applicationPort, pool)

-- | gen a 302 redirect helper
redirects :: (MonadError ServerError m) => BS.ByteString -> m a
redirects url = throwError err302 {errHeaders = [("Location", url)]}

-- TODO: A better way to extract userName from json response, probably using lens
genRandomUserName :: IO (Either BSL.ByteString Text)
genRandomUserName = do
  runReq defaultHttpConfig $ do
    jsonRes <-
      req
        Req.GET
        (https "randomuser.me" /: "api")
        NoReqBody
        jsonResponse
        mempty
    if responseStatusCode jsonRes /= 200
      then do
        liftIO $ print (BSL.pack (show (responseBody jsonRes)))
        pure $ Left (BSL.pack (show (responseBody jsonRes)))
      else do
        let res0 =
              randomUsername $
                head $
                  results (responseBody jsonRes :: RandomUserNameApiResponse)
        pure $ Right (T.pack res0)

threadToThreadInfo :: ThreadRead -> ThreadInfo
threadToThreadInfo Thread {..} =
  ThreadInfo
    { threadIDForThreadInfo = threadID,
      title = threadTitle,
      description = threadDescription,
      communityIDForThreadInfo = threadCommunityID,
      userIDForThreadInfo = threadUserID
    }
