module Main
  ( main
  )
  where

import Prelude

import Api.Request (BaseURL(..))
import AppM (runAppM)
import Component.Router (component)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Store (LogLevel(..))

main :: Effect Unit
main = do
  log "Running UI"
  HA.runHalogenAff do
     body <- HA.awaitBody
     let baseUrl = BaseURL "https://api.realworld.io"
         logLevel = LogDebug
         currentUser = Nothing -- Implementation will be written later.
         initStore = { baseUrl , logLevel , currentUser }
     rootComponent <- runAppM initStore component
     runUI rootComponent unit body
