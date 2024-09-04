module Page.Home where

import Prelude

import Capability.Resource (class ManageThreads, getThreads,class Navigate, navigate)
import Common.Types (PaginatedArray, Thread, Profile, MyRoute(..))
import Data.Array (mapWithIndex)
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Undefined (undefined)

type State = {
        threads :: RemoteData String (PaginatedArray Thread),
        currentUser :: Maybe Profile
    }

data Action = Initialize | LoadThreads | GoToLogin

component :: forall query output m. 
    MonadAff m =>
    Navigate m =>
    MonadStore Store.Action Store.Store m =>
    ManageThreads m => H.Component query Unit output m
component = connect (selectEq _.currentUser) $ H.mkComponent {
        initialState,
        render,
        eval : H.mkEval H.defaultEval {
            initialize = Just Initialize,
            handleAction = handleAction
        }
    }
  where
    initialState  { context: currentUser } = { threads : NotAsked, currentUser }

    render :: forall slots. State -> H.ComponentHTML Action slots m 
    render state = HH.div_ [ 
            HH.text "Home Pages :)",
            HH.br_,
            HH.text $ "user: " <> show state.currentUser,
            HH.br_,
            HH.button [ HE.onClick \_ -> GoToLogin ] [ HH.text "Go to login" ],
            HH.br_,
            threadList state.threads
        ]

    handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
    handleAction = case _ of
            Initialize -> do
               void $ H.fork $ handleAction LoadThreads

            LoadThreads -> do
               H.modify_ _ { threads = Loading }
               mThreadList <- getThreads
               H.modify_ _ { threads = fromMaybe mThreadList }
            
            GoToLogin -> navigate Login

threadList :: forall props act. 
    RemoteData String (PaginatedArray Thread) ->
    HH.HTML props act
threadList = case _ of
    NotAsked ->
        HH.text "Threads not loaded yet..."
    Loading ->
        HH.text "Loading..."
    Failure err ->
        HH.text ("Error loading threads: " <> err)
    Success { body } | length body == 0 ->
        HH.text "No threads are here...yet!"
    Success threads ->
        HH.div_ (threadPreview `mapWithIndex` threads.body)

threadPreview :: forall props act. Int -> Thread -> HH.HTML props act
threadPreview _ thread =
    HH.div_ [
        HH.text thread.title
    ]
