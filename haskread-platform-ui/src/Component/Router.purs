module Component.Router where

import Prelude

import Bulma.Common as B
import Bulma.Components.Navbar as B
import Bulma.Layout.Layout as B
import Capability.Resource (class ManageComments, class ManageCommunity, class ManageThreads, class ManageUser, class Navigate, navigate)
import Utils.Bulma (class_,classes_)
import Common.Types (myRoute, MyRoute(..), Profile, Pagination)
import Common.Utils (defaultPagination)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Page.ChangePassword as ChangePassword
import Page.CreateThread as CreateThread
import Page.DeleteUser as DeleteUser
import Page.Home as Home
import Page.Login as Login
import Page.OTP as OTP
import Page.Register as Register
import Page.UpdateThread as UpdateThread
import Page.ViewThread as ViewThread
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Store as Store
import Type.Proxy (Proxy(..))

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
  => ManageCommunity m
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
        Left e -> (log $ "err" <> show e) *> navigate (Home defaultPagination)
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
  render { route } =
    HH.div [ classes_ [B.container] ] [
    case route of
      Just r -> case r of
        Home p -> HH.slot_ (Proxy :: _ "home") unit Home.component { pagination_ : p}
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
    ]
