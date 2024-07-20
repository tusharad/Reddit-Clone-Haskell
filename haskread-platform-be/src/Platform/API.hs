{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Platform.API (
    MainAPI
  , mainServer
) where
import Platform.Handler

import Servant

mainServer :: Server MainAPI
mainServer = checkHealthH

type MainAPI = CheckHealthAPI

type CheckHealthAPI = "check-health" :> Get '[JSON] String