{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.CallbackInternal (callbackInternalPage) where

import Data.Text (Text)
import Effectful
import Platform.Common.Request
import Platform.Common.Types (AuthData (..), LoginUserResponse(..))
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData

newtype ViewCallbackInternalId = ViewCallbackInternalId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView ViewCallbackInternalId es where
  data Action ViewCallbackInternalId = DoRedirect Url | VerifyToken Text Text
    deriving (Show, Read, ViewAction)

  update (DoRedirect u) = redirect u
  update (VerifyToken state_ code_) = do
    eRes <- liftIO $ verifyOAuth state_ code_
    case eRes of
      Left _ -> redirect "/login"
      Right LoginUserResponse{..} -> do
        saveSession @AuthData (AuthData $ Just jwtToken)
        redirect "/"

data AuthStatus = Success Text Text | SomethingWentWrong
  deriving (Show, Eq)

authStatusPage :: AuthStatus -> View ViewCallbackInternalId ()
authStatusPage (Success state_ code_) = do
  el (onLoad (VerifyToken state_ code_) 100) $ do
    el_ "Processing OAuth..."
authStatusPage SomethingWentWrong = do
  el (onLoad (DoRedirect "/register") 100) $ do
    el_ "Authentication failed, redirecting..."

callbackInternalPage :: (Hyperbole :> es) => Eff es (Page '[ViewCallbackInternalId])
callbackInternalPage = do
  mbState <- lookupParam (Param "state")
  mbCode <- lookupParam (Param "code")
  case (mbState, mbCode) of
    (Just state_, Just code_) ->
      pure $ hyper (ViewCallbackInternalId 1) (authStatusPage (Success state_ code_))
    _ -> pure $ hyper (ViewCallbackInternalId 1) (authStatusPage SomethingWentWrong)
