module Page.ViewThread where

import Prelude

import Capability.Resource (class ManageComments, class ManageThreads, class Navigate, getCommentsByThreadID, getThread)
import Common.Types (Profile, ThreadInfo, PaginatedArray, NestedComment)
import Common.Utils (toThreadInfo_)
import Component.Footer as Footer
import Component.Header as Header
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Component.ThreadView as ThreadView

type Input = { threadID :: Int }

type State = {
    currentUser :: Maybe Profile
  , thread :: RemoteData String ThreadInfo
  , nestedComments :: RemoteData String (PaginatedArray NestedComment)
  , threadID :: Int
 }

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = (header :: OpaqueSlot Unit, footer :: OpaqueSlot Unit, communityList :: OpaqueSlot Unit, threadView :: OpaqueSlot Unit)


data Action = Initialize
            | LoadThread Int
            | LoadComments Int

component :: forall query output m.
    MonadAff m =>
    ManageThreads m =>
    ManageComments m =>
    Navigate m =>
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

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { thread, nestedComments } = 
      HH.div_ [
        HH.slot_ (Proxy :: _ "header") unit Header.component unit
        , HH.text "View Thread"
        , case thread of
            NotAsked -> HH.div_ []
            Loading -> HH.div_ [ HH.text "Loading..." ]
            Failure _ -> HH.div_ [ HH.text "failed to load Thread" ]
            Success t -> HH.div_ [ 
                    HH.text t.title,
                    HH.text $ show t.threadIDForThreadInfo,
                    HH.slot_ (Proxy :: _ "threadView") unit ThreadView.component { thread: t }
            ]
        , commentList nestedComments
        , HH.slot_ (Proxy :: _ "footer") unit Footer.component unit
      ]

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
