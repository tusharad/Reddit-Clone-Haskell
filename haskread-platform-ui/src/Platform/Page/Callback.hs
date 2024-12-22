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

import Data.Text
import Effectful
import Web.Hyperbole

newtype ViewCallbackId = ViewCallbackId Int
  deriving (Show, Read, ViewId)

instance HyperView ViewCallbackId es where
  data Action ViewCallbackId = DoRedirect
    deriving (Show, Read, ViewAction)

  update DoRedirect = redirect "/"

someView :: View ViewCallbackId ()
someView = do
  el (onLoad DoRedirect 10) $ do
    el_ "Callback page"

callbackPage :: (Hyperbole :> es) => Eff es (Page '[ViewCallbackId])
callbackPage = do
  p :: Text <- reqParam "token"
  setSession "jwt_token" p
  pure $ col (pad 20) $ do
    hyper (ViewCallbackId 1) someView
