module Main
  ( main
  )
  where

import Prelude

import AppM (runAppM)
import Component.Router (component)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Class (liftEffect)
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Halogen.Aff as HA
import Halogen as H
import Component.Router as Router
import Effect.Aff (launchAff_,Aff)
import Halogen.VDom.Driver (runUI)
import Common.Types (myRoute,BaseURL(..),Endpoint(..),RequestMethod(..),profileCodec,Profile)
import Common.Utils (readToken,defaultRequest)
import Affjax.Web (request)
import Data.Either (Either(..),hush)
import Data.Codec.Argonaut as CA
import Store as Store
import Undefined (undefined)
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Codec as Codec

main :: Effect Unit
main = do
  log "Running UI"
  HA.runHalogenAff do
     body <- HA.awaitBody
     let baseUrl = BaseURL "http://localhost:8085"
     currentUser <- getCurrentUser baseUrl
     let initStore = { baseUrl , currentUser }
     rootComponent <- runAppM initStore component
     halogenIO <- runUI rootComponent unit body
     void $ liftEffect $ matchesWith (parse myRoute) \old new ->
       when (old /= Just new) $ launchAff_ do
          _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
          pure unit

getCurrentUser :: BaseURL -> Aff (Maybe Profile)
getCurrentUser baseUrl = do
    mToken <- liftEffect readToken
    case mToken of
        Nothing -> pure Nothing
        Just token -> do
           let requestOptions = { endpoint: UserByToken, method: Get }
           res <- request $ defaultRequest baseUrl (Just token) requestOptions
           let
               user :: Either String Profile
               user = case res of
                Left e ->
                    Left "error fetching request"
                Right v -> lmap printJsonDecodeError do
                    CA.decode profileCodec v.body
           pure $ hush user
