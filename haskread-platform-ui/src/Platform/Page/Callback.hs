{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.Callback (callbackPage) where

import Data.Text (Text)
import Effectful
import Platform.Common.Types (AuthData (..))
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData

newtype ViewCallbackId = ViewCallbackId Int
  deriving (Show, Read, ViewId)

instance HyperView ViewCallbackId es where
  data Action ViewCallbackId = DoRedirect Url
    deriving (Show, Read, ViewAction)

  update (DoRedirect u) = redirect u

data AuthStatus = Success | TokenNotFound
  deriving (Show, Eq)

authStatusPage :: AuthStatus -> View ViewCallbackId ()
authStatusPage Success = do
  el (onLoad (DoRedirect "/") 100) $ do
    el_ "Authentication success, redirecting..."
authStatusPage TokenNotFound = do
  el (onLoad (DoRedirect "/login") 100) $ do
    el_ "Authentication success, redirecting..."

callbackPage :: (Hyperbole :> es) => Eff es (Page '[ViewCallbackId])
callbackPage = do
  mbToken <- lookupParam (Param "token")
  case mbToken of
    Nothing -> pure $ hyper (ViewCallbackId 1) (authStatusPage TokenNotFound)
    Just (token :: Text) -> do
      saveSession (AuthData (Just token))
      pure $ hyper (ViewCallbackId 1) (authStatusPage Success)
