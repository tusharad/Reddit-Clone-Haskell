{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Platform.API (
    MainAPI
  , mainServer
) where

import Platform.Handler
import Platform.User.Handler
import Platform.Common.AppM
import Platform.User.Types

import Servant
import UnliftIO

mainServer :: MonadUnliftIO m => ServerT MainAPI (AppM m)
mainServer = checkHealthH
        :<|> registerUserH

type MainAPI = CheckHealthAPI
         :<|> RegisterUserAPI

type CheckHealthAPI = "check-health" :> Get '[JSON] String

-- User APIs
type RegisterUserAPI = "api" 
  :> "v1" 
    :> "user" 
      :> "auth" 
        :> "register" 
          :> ReqBody '[JSON] RegisterUserBody 
            :> Post '[JSON] RegisterUserResponse
