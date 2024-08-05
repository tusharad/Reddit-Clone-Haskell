{-# LANGUAGE DataKinds #-}
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
  )
where

import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Password.Bcrypt
import Data.String.Interpolate
import qualified Data.Text as T
import Dhall
import Haxl.Core (initEnv, stateEmpty, stateSet)
import qualified Orville.PostgreSQL as O
import Orville.PostgreSQL.Raw.Connection
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Types
import Platform.DB.Model
import Platform.Haxl.DataSource
import Servant
import Servant.Auth.Server
import System.Log.FastLogger

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
    { userPassword = MyPassword password0,
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
          jwtSecretKey <- generateKey
          loggerSet_ <- newFileLoggerSet defaultBufSize logFilePath
          sem <- newQSem 10 -- 10 threads
          haxlEnv0 <- initEnv (stateSet (HaskReadState pool sem) stateEmpty) ()
          let orvilleState = O.newOrvilleState O.defaultErrorDetailLevel pool
              appST =
                MyAppState
                  (AppConfig fileUploadPath loggerSet_ (toLogLevel logLevel))
                  orvilleState
                  haxlEnv0
              jwtSett = defaultJWTSettings jwtSecretKey
              ctx = defaultCookieSettings :. jwtSett :. EmptyContext
          pure $ Right (appST, jwtSett, ctx, fromIntegral applicationPort, pool)
