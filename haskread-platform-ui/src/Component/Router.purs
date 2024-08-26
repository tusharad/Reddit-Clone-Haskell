module Component.Router
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Common.Types (myRoute,MyRoute(..),Profile)
import Data.Maybe (Maybe(..))
import Routing.Duplex as RD
import Data.Either (Either(..))
import Effect.Class.Console (log)
import Effect.Class (class MonadEffect,liftEffect)
import Capability.Navigate (class Navigate,navigate)
import Capability.Resource (class ManageThreads,class ManageUser)
import Effect.Aff.Class (class MonadAff)
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Common.Utils (readToken)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Select (selectEq)
import Store as Store
import Halogen.Store.Monad (class MonadStore)

data Action = Initialize
        | Receive (Connected (Maybe Profile) Unit)

type State = {
        route :: Maybe MyRoute,
        currentUser :: Maybe Profile
    }
data Query a = Navigate MyRoute a 

type OpaqueSlot slot = forall query. H.Slot query Void slot

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  )

component :: forall m.
            MonadStore Store.Action Store.Store m =>
            Navigate m => 
            ManageThreads m => 
            MonadAff m => 
            ManageUser m =>
            MonadEffect m => H.Component Query Unit Void m
component = connect (selectEq _.currentUser) $ H.mkComponent {
        initialState 
      , render
      , eval : H.mkEval H.defaultEval {
        initialize = Just Initialize,
               receive = Just <<< Receive,
        handleAction = handleAction,
        handleQuery = handleQuery
      }
    }
  where
    initialState { context: currentUser } = { route : Nothing, currentUser : currentUser }

    handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
            Initialize -> do
                url <- liftEffect getHash
                case RD.parse myRoute url of
                    Left e -> (log $ "err" <> show e) *> navigate Home
                    Right r -> navigate r
            Receive { context: currentUser } -> do
                log $ "user" <> show currentUser
                H.modify_ _ { currentUser = currentUser }

    handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
            Navigate dest a -> do
              H.modify_ _ { route = Just dest }
              pure (Just a)

    render :: State -> H.ComponentHTML Action ChildSlots m 
    render {route} = case route of  
                Just r -> case r of
                    Home -> HH.slot_ (Proxy :: _ "home") unit Home.component unit
                    Login -> HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: true }
                    Register -> HH.slot_ (Proxy :: _ "register") unit Register.component { redirect: true }
                Nothing -> HH.div_ [ HH.text "page not found!" ]
