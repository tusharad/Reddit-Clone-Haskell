module Page.Home where

import Prelude

import Capability.Navigate (class Navigate)
import Capability.Resource.Thread (class ManageThread, getThreads)
import Data.Maybe (Maybe(..))
import Data.Profile (Profile)
import Data.Thread (Thread, PaginatedArray)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Store as Store
import Undefined (undefined)

data Action
    = Initialize
    | Receive (Connected (Maybe Profile) Unit)
    | LoadThreads 

type State = {
    currentUser :: Maybe Profile,
    threads :: RemoteData String (PaginatedArray Thread)
 }

component :: 
    forall q o m. 
    MonadAff m => 
    MonadStore Store.Action Store.Store m => 
    Navigate m => 
    ManageThread m => 
    H.Component q Unit o m 
component = connect (selectEq _.currentUser) $ H.mkComponent
    {
        initialState,
        render,
        eval : H.mkEval H.defaultEval {
            handleAction = handleAction
          , receive = Just <<< Receive
          , initialize = Just Initialize 
        }
    }
  where
    initialState {context : currentUser} =
        {
          threads : NotAsked
        , currentUser
        }
    
    handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
    handleAction = case _ of
        Initialize -> do
            void $ H.fork $ handleAction LoadThreads

        Receive { context: currentUser } ->
            H.modify_ _ { currentUser = currentUser }

        LoadThreads -> do
            H.modify_ _ { threads = Loading }
            articles <- getThreads
            H.modify_ _ { threads = fromMaybe articles }
    
    render :: forall slots. State -> H.ComponentHTML Action slots m
    render state@{threads,currentUser} = 
        HH.div_ [ HH.text "Hi" ]