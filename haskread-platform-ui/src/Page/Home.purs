module Page.Home where

import Prelude
import Undefined (undefined)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..),fromMaybe)
import Common.Types (PaginatedArray,Thread)
import Capability.Resource (class ManageThreads,getThreads)
import Data.Foldable (length)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))

type State = {
        threads :: RemoteData String (PaginatedArray Thread)
    }

data Action = Initialize | LoadThreads

component :: forall query input output m. ManageThreads m => H.Component query input output m
component = H.mkComponent {
        initialState,
        render,
        eval : H.mkEval H.defaultEval {
            initialize = Just Initialize,
            handleAction = handleAction
        }
    }
  where
    initialState :: input -> State
    initialState _ = { threads : NotAsked }

    render :: forall slots. State -> H.ComponentHTML Action slots m 
    render state = HH.div_ [ 
            HH.text "Home Page",
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