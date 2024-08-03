{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.Core (startApp, app) where

-- Starting point of the Application

import Control.Monad.Reader
import Network.Wai.Handler.Warp
import Platform.API
import Platform.Common.AppM
import Platform.Common.Types
import Platform.Common.Utils
import Servant
import Servant.Auth.Server
import System.Directory
import System.Environment
import System.Exit

runAppM :: MyAppState -> AppM IO a -> Handler a
runAppM myAppState appM = Handler $ runMyExceptT $ runReaderT (getApp appM) myAppState

allServer :: CookieSettings -> JWTSettings -> MyAppState -> Server (MainAPI auths)
allServer cookieSett jwtSett myAppState =
  hoistServerWithContext
    (Proxy :: Proxy (MainAPI '[JWT, Cookie]))
    (Proxy :: Proxy '[CookieSettings, JWTSettings])
    (runAppM myAppState)
    ( mainServer
        cookieSett
        jwtSett
    )

startApp :: IO ()
startApp = do
  argList <- getArgs
  if (null argList)
    then (putStrLn "please provide argument") >> exitFailure
    else do
      let envFilePath = head argList
      fileExists <- doesFileExist envFilePath
      if not fileExists
        then (putStrLn "please provide argument") >> exitFailure
        else do
          eEnv <- readEnv envFilePath
          case eEnv of
            Left e -> (putStrLn $ show e) >> exitFailure
            Right (appST, jwtSett, ctx, appPort, _) -> do
              putStrLn $ "Application running at pot " <> show appPort
              run appPort (app appST jwtSett ctx)

app :: MyAppState -> JWTSettings -> Context [CookieSettings, JWTSettings] -> Application
app appST jwtSett ctx =
  serveWithContext
    (Proxy :: Proxy (MainAPI '[JWT, Cookie]))
    ctx
    (allServer defaultCookieSettings jwtSett appST)
