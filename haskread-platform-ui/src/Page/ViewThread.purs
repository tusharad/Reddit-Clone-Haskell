module Page.ViewThread where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Common.Types (Profile,ThreadInfo,PaginatedArray,NestedComment)
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectEq)
import Capability.Resource ( 
      class ManageThreads
    , getThread
    , class ManageComments
    , getCommentsByThreadID
  )
import Halogen.Store.Monad (class MonadStore)
import Store as Store
import Undefined (undefined)
import Common.Utils (toThreadInfo_)
import Effect.Class (liftEffect)
import Data.Newtype (unwrap)

type Input = { threadID :: Int }

type State = {
    currentUser :: Maybe Profile
  , thread :: RemoteData String ThreadInfo
  , nestedComments :: RemoteData String (PaginatedArray NestedComment)
  , threadID :: Int
 }

data Action = Initialize
            | LoadThread Int
            | LoadComments Int

component :: forall query output m.
    MonadAff m =>
    ManageThreads m =>
    ManageComments m =>
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
             , nestedComments : NotAsked
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
            H.modify_ _ { nestedComments = Loading }
            mNestedComments <- getCommentsByThreadID threadID
            H.modify_ _ { nestedComments = fromMaybe mNestedComments }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { thread, nestedComments } = 
      HH.div_ [
        HH.text "View Thread"
        , threadView thread
        , commentList nestedComments
      ]

  threadView :: forall props act. RemoteData String ThreadInfo -> HH.HTML props act
  threadView = 
      case _ of
          NotAsked -> HH.div_ []
          Loading -> HH.div_ [ HH.text "Loading..." ]
          Failure _ -> HH.div_ [ HH.text "failed to load Thread" ]
          Success t -> HH.div_ [ 
                    HH.text t.title,
                    HH.text $ show t.threadIDForThreadInfo]

  commentList :: forall props act. RemoteData String (PaginatedArray NestedComment) ->
                    HH.HTML props act
  commentList =
      case _ of
        NotAsked -> HH.div_ []
        Loading -> HH.div_ [ HH.text "Loading..." ]
        Failure _ -> HH.div_ [ HH.text "failed to load comments" ]
        Success cs -> HH.div_ [
           HH.text $ show $ (\c -> (show (unwrap c).mainComment)) `map` cs.body
        ]

