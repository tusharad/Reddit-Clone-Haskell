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

import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Platform.Page.Callback
import Platform.Page.Home
import Platform.Page.Login
import Platform.Page.OTP
import Platform.Page.Profile
import Platform.Page.Register
import Platform.Page.ViewThread
import Text.Read (readMaybe)
import Web.Hyperbole

main :: IO ()
main = do
  putStrLn "UI running on http://localhost:3000"
  run 3000 $ do
    liveApp (basicDocument "HaskRead-UI") (routeRequest router)

data AppRoute
  = Home
  | Login
  | Register
  | ViewThread Int
  | OTP Int
  | Callback
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
  matchRoute _ = pure Home

router :: forall es. (Hyperbole :> es, IOE :> es) => AppRoute -> Eff es Response
router Home = runPage homePage
router Login = runPage loginPage
router Register = runPage registerPage
router Profile = runPage profilePage
router Callback = runPage callbackPage
router (OTP tId) = runPage (otpPage tId)
router (ViewThread tId) = runPage (viewThreadPage tId)

newtype Message = Message Int
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message
    = Louder Text
    | Softer Text
    deriving (Show, Read, ViewAction)

  update (Louder m) = do
    let new = m <> "!"
    pure $ messageView new
  update (Softer m) = do
    if not (T.null m) && T.last m == '!'
      then do
        let new = T.init m
        pure $ messageView new
      else
        pure $ messageView m

messageView :: Text -> View Message ()
messageView m = do
  el_ $ text m
  button (Louder m) (border 1) "Louder"
  button (Softer m) (border 1) "Softer"
