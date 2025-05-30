{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.Core (startApp, app) where

-- Starting point of the Application
import Control.Monad.Reader
import Network.Wai.Handler.Warp
import Platform.API
import Platform.Common.AppM
import Platform.Common.Middleware
import Platform.Common.Types
import Platform.Common.Utils
import Servant
import Servant.Auth.Server
import System.Directory
import System.Environment
import System.Exit

runAppM :: MyAppState -> AppM IO a -> Handler a
runAppM myAppState appM = Handler $ runMyExceptT $ runReaderT (getApp appM) myAppState

allServer :: MyAppState -> Server (MainAPI auths)
allServer myAppState =
  hoistServerWithContext
    (Proxy :: Proxy (MainAPI '[JWT, Cookie]))
    (Proxy :: Proxy '[CookieSettings, JWTSettings])
    (runAppM myAppState)
    mainServer

startApp :: IO ()
startApp = do
  argList <- getArgs
  case argList of
    (envFilePath : _) -> do
      fileExists <- doesFileExist envFilePath
      if not fileExists
        then putStrLn "please provide argument" >> exitFailure
        else do
          eEnv <- readEnv envFilePath
          case eEnv of
            Left e -> do
                putStrLn ("error happened: " <> show e)
                exitFailure
            Right (appST, _, ctx, appPort, _) -> do
              -- initRateLimitTable "rate_limit.db"
              putStrLn $ "Application running at pot " <> show appPort
              run appPort $
                concatMiddleware
                  [ myCorsMiddleware
                  -- , rateLimitMiddleware 60 60 -- At max 60 requests in 1 minute
                  ]
                  (app appST ctx)
    _ -> putStrLn "please provide argument" >> exitFailure

app :: MyAppState -> Context [CookieSettings, JWTSettings] -> Application
app appST ctx =
  serveWithContext
    (Proxy :: Proxy (MainAPI '[JWT, Cookie]))
    ctx
    (allServer appST)
