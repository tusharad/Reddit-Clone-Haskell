module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Component.Router (rootComponent)

main :: Effect Unit
main = do
  log "Running UI"
  HA.runHalogenAff do
     body <- HA.awaitBody
     runUI rootComponent unit body
