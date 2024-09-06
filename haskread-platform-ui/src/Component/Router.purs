module Component.Router where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Common.Types (myRoute, MyRoute(..), Profile)
import Data.Maybe (Maybe(..), isNothing)
import Data.Foldable (elem)
import Routing.Duplex as RD
import Data.Either (Either(..))
import Effect.Class.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Capability.Resource (
    class ManageThreads
    , class ManageUser
    , class Navigate
    , class ManageComments
    , navigate
    )
import Effect.Aff.Class (class MonadAff)
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Select (selectEq)
import Store as Store
import Halogen.Store.Monad (class MonadStore)
-- Pages
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Page.OTP as OTP
import Page.CreateThread as CreateThread
import Page.ChangePassword as ChangePassword
import Page.DeleteUser as DeleteUser
import Page.UpdateThread as UpdateThread
import Page.ViewThread as ViewThread

data Action
  = Initialize
  | Receive (Connected (Maybe Profile) Unit)

type State =
  { route :: Maybe MyRoute
  , currentUser :: Maybe Profile
  }

data Query a = Navigate MyRoute a

type OpaqueSlot slot = forall query. H.Slot query Void slot

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  , otp :: OpaqueSlot Unit
  , createThread :: OpaqueSlot Unit
  , changePassword :: OpaqueSlot Unit
  , deleteUser :: OpaqueSlot Unit
  , updateThread :: OpaqueSlot Unit
  , viewThread :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageThreads m
  => MonadAff m
  => ManageUser m
  => ManageComments m
  => MonadEffect m
  => H.Component Query Unit Void m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  authRoute = [ CreateThread, ChangePassword, DeleteUser, UpdateThread 1 ]

  initialState { context: currentUser } = { route: Nothing, currentUser: currentUser }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      url <- liftEffect getHash
      case RD.parse myRoute url of
        Left e -> (log $ "err" <> show e) *> navigate Home
        Right r -> navigate r
    Receive { context: currentUser } -> do
      H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { currentUser } <- H.get
      unless (isNothing currentUser && dest `elem` authRoute)
        (H.modify_ _ { route = Just dest })
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home -> HH.slot_ (Proxy :: _ "home") unit Home.component unit
      Login -> HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: true }
      Register ->
        HH.slot_ (Proxy :: _ "register") unit Register.component { redirect: true }
      OTP uID -> HH.slot_ (Proxy :: _ "otp") unit OTP.component { userID: uID }
      CreateThread ->
        HH.slot_
          (Proxy :: _ "createThread")
          unit
          CreateThread.component
          unit
      ChangePassword ->
        HH.slot_ (Proxy :: _ "changePassword") unit ChangePassword.component unit
      DeleteUser ->
        HH.slot_ (Proxy :: _ "deleteUser") unit DeleteUser.component unit
      UpdateThread threadID ->
        HH.slot_
          (Proxy :: _ "updateThread")
          unit
          UpdateThread.component
          { threadID: threadID }
      ViewThread threadID -> HH.slot_ (Proxy :: _ "viewThread") unit 
                                ViewThread.component { threadID : threadID }
    Nothing -> HH.div_ [ HH.text "page not found!" ]
