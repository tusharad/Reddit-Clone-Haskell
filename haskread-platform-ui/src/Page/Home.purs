module Page.Home where

import Prelude
import Undefined (undefined)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..),fromMaybe)
import Common.Types (PaginatedArray,Thread)
import Capability.Resource (class ManageThreads,getThreads)
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

    render :: forall state slots. state -> H.ComponentHTML Action slots m 
    render _ = HH.div_ [ HH.text "Home Page" ]

    handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
    handleAction = case _ of
            Initialize -> do
               void $ H.fork $ handleAction LoadThreads

            LoadThreads -> do
               H.modify_ _ { threads = Loading }
               mThreadList <- getThreads
               H.modify_ _ { threads = fromMaybe mThreadList }
