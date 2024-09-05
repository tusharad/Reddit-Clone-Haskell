module Page.Home where

import Prelude

import Capability.Resource (class ManageThreads, getThreads, class Navigate, navigate)
import Common.Types (PaginatedArray, Thread, Profile, MyRoute(..), ThreadInfo)
import Data.Array (mapWithIndex)
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Undefined (undefined)
import Common.Utils (stringToDate, toThreadInfo, safeHref)
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties as HP

import Component.Header as Header
import Component.Footer as Footer

type State =
  { threads :: RemoteData String (PaginatedArray ThreadInfo)
  , currentUser :: Maybe Profile
  }

data Action = Initialize | LoadThreads | GoToLogin

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = ( header :: OpaqueSlot Unit, footer :: OpaqueSlot Unit )

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => ManageThreads m
  => H.Component query Unit output m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }
  where
  initialState { context: currentUser } = { threads: NotAsked, currentUser }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div_
    [ 
      HH.slot_ (Proxy :: _ "header") unit Header.component unit
    , HH.text "Home Pages :)"
    , HH.br_
    , HH.text $ "user: " <> show state.currentUser
    , HH.br_
    , HH.button [ HE.onClick \_ -> GoToLogin ] [ HH.text "Go to login" ]
    , HH.br_
    , threadList state.threads
    , HH.slot_ (Proxy :: _ "footer") unit Footer.component unit
    ]

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadThreads

    LoadThreads -> do
      _ <- H.modify_ _ { threads = Loading }
      mThreadList <- getThreads
      case mThreadList of
        Nothing ->
          H.modify_ _ { threads = Failure "asdas" }
        Just threadList -> do
          threadInfoList <- liftEffect $ toThreadInfo threadList
          H.modify_ _ { threads = fromMaybe (Just threadInfoList) }

    GoToLogin -> navigate Login

threadList
  :: forall props act
   . RemoteData String (PaginatedArray ThreadInfo)
  -> HH.HTML props act
threadList = case _ of
  NotAsked ->
    HH.text "Threads not loaded yet..."
  Loading ->
    HH.text "Loading..."
  Failure err ->
    HH.text ("Error loading threads: " <> err)
  Success { body } | length body == 0 ->
    HH.text "No threads are here...yet!"
  Success threads -> do
    HH.div_ (threadPreview `mapWithIndex` threads.body)

threadPreview :: forall props act. Int -> ThreadInfo -> HH.HTML props act
threadPreview _ thread =
  HH.div_
    [ 
     HH.text thread.userNameForThreadInfo
    , HH.text thread.communityNameForThreadInfo
    , HH.a [ safeHref (ViewThread thread.threadIDForThreadInfo) ] [
         HH.text thread.title
        , HH.br_
        , HH.text $ Maybe.fromMaybe "" thread.description]
    , HH.br_
    , HH.text $ show $ Maybe.fromMaybe 0 thread.upvoteCount
    , HH.text $ show $ Maybe.fromMaybe 0 thread.downvoteCount
    , HH.text $ show $ Maybe.fromMaybe 0 thread.commentCount
    , HH.br_
    , HH.text $ Maybe.fromMaybe "aasdsa" thread.age
    ]
