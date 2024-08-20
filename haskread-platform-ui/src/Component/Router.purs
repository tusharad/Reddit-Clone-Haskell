module Component.Router
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Undefined

import Common.Types (myRoute,MyRoute(..))
import Data.Maybe (Maybe(..))
import Routing.Duplex as RD
import Data.Either (Either(..))

import Effect.Class.Console (log)
import Effect.Class (class MonadEffect,liftEffect)
import Capability.Navigate (class Navigate,navigate)
import Capability.Resource (class ManageThreads)

import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Page.Home as Home
import Page.Login as Login

data Action = Initialize
type State = {
        route :: Maybe MyRoute
    }
data Query a = Navigate MyRoute a 

type OpaqueSlot slot = forall query. H.Slot query Void slot

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  )

component :: 
    forall input m. 
            Navigate m => 
            ManageThreads m => 
            MonadEffect m => H.Component Query input Void m
component = H.mkComponent {
        initialState 
      , render
      , eval : H.mkEval H.defaultEval {
        initialize = Just Initialize,
        handleAction = handleAction,
        handleQuery = handleQuery
      }
    }
  where
    initialState :: input -> State
    initialState _ = { route : Nothing }

    handleAction :: forall state. Action -> H.HalogenM state Action ChildSlots Void m Unit
    handleAction = case _ of
            Initialize -> do
                url <- liftEffect getHash
                case RD.parse myRoute url of
                    Left e -> log $ "err" <> show e
                    Right r -> navigate r
                log "initializing"

    handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
            Navigate dest a -> do
              liftEffect $ log "inside handleQuery"
              H.modify_ _ { route = Just dest }
              pure (Just a)

    render :: State -> H.ComponentHTML Action ChildSlots m 
    render {route} = case route of  
                Just r -> case r of
                              Home -> HH.slot_ (Proxy :: _ "home") unit Home.component unit
                              Login -> HH.slot_ (Proxy :: _ "login") unit Login.component unit
                Nothing -> HH.div_ [ HH.text "page not found!" ]
