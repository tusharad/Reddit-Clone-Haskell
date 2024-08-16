module Component.Router
  where

import Data.Route
import Prelude

import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.Thread (class ManageThread)
import Data.Array (elem)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Profile (Profile)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Page.Home as Home
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Store as Store
import Type.Proxy (Proxy(..))
import Undefined (undefined)


data Query a = Navigate Route a

type State = {
   route :: Maybe Route,
   currentUser :: Maybe Profile
 }

data Action = Initialize | Receive (Connected (Maybe Profile) Unit)

type OpaqueSlot slot = forall query. H.Slot query Void slot

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , signup :: OpaqueSlot Unit
  )

component :: 
  forall m. 
  MonadAff m => 
  MonadStore Store.Action Store.Store m => 
  ManageThread m => 
  Navigate m => 
  H.Component Query Unit Void m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState: \{context : currentUser} -> { route : Nothing ,currentUser } 
    , render
    , eval: H.mkEval H.defaultEval {
      handleQuery = handleQuery,
      handleAction = handleAction,
      receive = Just <<< Receive,
      initialize = Just Initialize
    }
  }
  where
    handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
      Initialize -> do
        log "Here!"
        initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
        log $ "here as well" <> show initialRoute
        navigate $ fromMaybe Home initialRoute
      Receive { context : currentUser } -> 
        H.modify_ _ { currentUser = currentUser }
    
    handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
      Navigate dest a -> do
       log "reacheddd"
       { route , currentUser } <- H.get
       H.modify_ _ { route = Just dest }
       pure (Just a)
      _ -> log "not reachging Navigate" *> pure Nothing
    
    -- authorize :: Maybe Profile -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
    -- authorize mbProfile html = case mbProfile of
    --   Nothing ->
    --     undefined --HH.slot (Proxy :: _ "login") unit Login.component { redirect: false } absurd
    --   Just _ ->
    --     html
    
    render :: State -> H.ComponentHTML Action ChildSlots m
    render {route,currentUser} = case route of
      Just r -> case r of
        Home -> HH.slot_ (Proxy :: _ "home") unit Home.component unit
        _ -> HH.div_ [ HH.text "Oh no! That " ]
      Nothing ->  HH.div_ [ HH.text "Oh no!! That page wasn't found." ]
