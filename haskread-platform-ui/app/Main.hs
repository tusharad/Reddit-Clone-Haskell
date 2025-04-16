{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Data.Text as T
import Effectful
import Effectful.Reader.Dynamic
import Network.Wai.Middleware.Static
import Platform.Common.Types
import Platform.Page.Callback
import Platform.Page.CallbackInternal
import Platform.Page.Home
import Platform.Page.Login
import Platform.Page.OTP
import Platform.Page.Profile
import Platform.Page.Register
import Platform.Page.ViewThread
import System.Log.FastLogger
import Text.Read (readMaybe)
import Web.Hyperbole

main :: IO ()
main = do
  putStrLn "UI running on http://localhost:3000"
  loggerSet_ <- newStdoutLoggerSet defaultBufSize
  let appConfig =
        AppConfig
          { environment = Development
          , minLogLevel = LevelDebug
          , loggerSet = loggerSet_
          }
      app = staticPolicy (addBase "static") (myApp appConfig)
  run 3000 app

myApp :: AppConfig -> Application
myApp appConfig = liveApp (basicDocument "HaskRead-UI") (runReader appConfig $ routeRequest router)

data AppRoute
  = Home
  | Login
  | Register
  | ViewThread Int
  | OTP Int
  | Callback
  | CallbackInternal
  | Profile
  deriving (Eq, Generic)

instance Route AppRoute where
  baseRoute = Just Home
  matchRoute ["login"] = pure Login
  matchRoute ["profile"] = pure Profile
  matchRoute ["register"] = pure Register
  matchRoute ["oauth2", "callback"] = pure Callback
  matchRoute ["otp", newUserId_] = do
    tId_ <- readMaybe $ T.unpack newUserId_
    pure $ OTP tId_
  matchRoute ["view-thread", tId] = do
    tId_ <- readMaybe $ T.unpack tId
    pure $ ViewThread tId_
  matchRoute ["callback"] = pure CallbackInternal
  matchRoute _ = pure Home

router ::
  forall es. (Hyperbole :> es, IOE :> es, Reader AppConfig :> es) => AppRoute -> Eff es Response
router Home = runPage homePage
router Login = runPage loginPage
router Register = runPage registerPage
router Profile = runPage profilePage
router Callback = runPage callbackPage
router (OTP tId) = runPage (otpPage tId)
router (ViewThread tId) = runPage (viewThreadPage tId)
router CallbackInternal = runPage callbackInternalPage
