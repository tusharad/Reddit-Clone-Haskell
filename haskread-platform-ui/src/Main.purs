module Main
  ( main
  )
  where

import Prelude

import AppM (runAppM)
import Component.Router (component)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Common.Types (myRoute)
import Halogen.Aff as HA
import Halogen as H
import Component.Router as Router
import Effect.Aff (launchAff_)
import Halogen.VDom.Driver (runUI)
import Store (LogLevel(..),BaseURL(..))

main :: Effect Unit
main = do
  log "Running UI"
  HA.runHalogenAff do
     body <- HA.awaitBody
     let baseUrl = BaseURL "http://localhost:8001"
         logLevel = LogDebug
         currentUser = Nothing -- Implementation will be written later.
         initStore = { baseUrl , logLevel , currentUser }
     rootComponent <- runAppM initStore component
     halogenIO <- runUI rootComponent unit body
     void $ liftEffect $ matchesWith (parse myRoute) \old new ->
       when (old /= Just new) $ launchAff_ do
          _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
          pure unit
