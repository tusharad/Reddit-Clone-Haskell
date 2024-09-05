module Page.ViewThread where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Common.Types (Profile,ThreadInfo)
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectEq)
import Capability.Resource (class ManageThreads,getThread)
import Halogen.Store.Monad (class MonadStore)
import Store as Store
import Undefined (undefined)
import Common.Utils (toThreadInfo_)
import Effect.Class (liftEffect)

type Input = { threadID :: Int }

type State = {
    currentUser :: Maybe Profile
  , thread :: RemoteData String ThreadInfo
  , threadID :: Int
 }

data Action = Initialize
            | LoadThread Int
            | LoadComments Int

component :: forall query output m.
    MonadAff m =>
    ManageThreads m =>
    MonadStore Store.Action Store.Store m =>
    H.Component query Input output m
component = connect (selectEq _.currentUser) $ H.mkComponent {
    initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }
  where
  initialState { context: currentUser, input : { threadID } } = { 
               thread: NotAsked
             , currentUser 
             , threadID : threadID
            }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction =
    case _ of
        Initialize -> do
           { threadID } <- H.get
           void $ H.fork $ handleAction (LoadThread threadID)
           void $ H.fork $ handleAction (LoadComments threadID)
        
        LoadThread threadID -> do
           H.modify_ _ { thread = Loading }
           mThread <- getThread threadID
           case mThread of
               Just thread -> do
                  threadInfo <- liftEffect $ toThreadInfo_ thread
                  H.modify_ _ { thread = Success threadInfo }
               Nothing -> do
                  H.modify_ _ { thread = Failure "Get thread info failed" }
        
        LoadComments threadID -> do
            -- H.modify_ _ {  }
            undefined

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser } = 
      HH.div_ [
        HH.text "View Thread"
      ]
