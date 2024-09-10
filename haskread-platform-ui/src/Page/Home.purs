module Page.Home where

import Prelude

import Bulma.CSS.Spacing as B
import Bulma.Columns.Columns as B
import Bulma.Columns.Size as B
import Bulma.Components.Tabs as B
import Bulma.Elements.Button as B
import Bulma.Form.Common as B
import Bulma.Form.General as B
import Bulma.Modifiers.Typography as B
import Bulma.Elements.Elements as B
import Capability.Resource (class ManageThreads, getThreads, class Navigate, navigate, class ManageCommunity)
import Common.Types (MyRoute(..), PaginatedArray, Profile, Thread, ThreadInfo, Pagination)
import Common.Utils (stringToDate, toThreadInfo, safeHref)
import Component.CommunityList as CommunityList
import Component.Footer as Footer
import Component.Header as Header
import Component.ThreadView as ThreadView
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
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties as HP
import Utils.Bulma (class_, classes_)

type Input =
  { pagination_ :: Pagination
  }

type State =
  { threads :: RemoteData String (PaginatedArray ThreadInfo)
  , currentUser :: Maybe Profile
  , pagination_ :: Pagination
  }

data Action = Initialize | LoadThreads | GoToLogin

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = (header :: OpaqueSlot Unit, footer :: OpaqueSlot Unit, communityList :: OpaqueSlot Unit, threadView :: OpaqueSlot Unit)

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => ManageThreads m
  => ManageCommunity m
  => H.Component query Input output m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }
  where
  initialState { context: currentUser, input: { pagination_ } } =
    { threads: NotAsked, currentUser, pagination_ }

  someFunc textVal iconVal = HH.li [ class_ B.isActive ]
    [ HH.a_
        [ HH.span [ classes_ [ B.icon, B.isSmall ] ]
            [ HH.i [ HP.class_ $ HH.ClassName iconVal ] [] ]
        , HH.span_ [ HH.text textVal ]
        ]
    ]

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div_
    [ HH.slot_ (Proxy :: _ "header") unit Header.component unit
    -- , HH.text "Home Pages :)"
    -- , HH.br_
    -- , HH.text $ "user: " <> show state.currentUser
    -- , HH.br_
    , HH.div [ classes_ [ B.columns, B.is8, B.py5 ] ]
        [ HH.div [ class_ B.column ]
            [ HH.p [ classes_ [ B.isSize3, B.hasTextCentered, B.pb4 ] ] [ HH.text "Threads" ]
            , HH.div [ classes_ [ B.tabs, B.isCentered, B.isRounded ] ]
                [ HH.ul_
                    [ someFunc "Top Voted" "bx bxs-objects-vertical-top"
                    , someFunc "Trending" "bx bx-trending-up"
                    , someFunc "New" "bx bx-polygon"
                    , someFunc "Following" "bx bx-run"
                    ]
                ]
            , case state.threads of
                NotAsked -> HH.text "Threads not loaded yet..."
                Loading -> HH.text "Loading..."
                Failure err -> HH.text ("Error loading threads: " <> err)
                Success { body } | length body == 0 -> HH.text "No threads are here...yet!"
                Success threads -> do
                  HH.div_
                    ( ( \thread ->
                          HH.slot_
                            (Proxy :: _ "threadView")
                            unit
                            ThreadView.component
                            { thread: thread }
                      )
                        `map` threads.body
                    )
            ]
        ]

    -- , HH.slot_ (Proxy :: _ "communityList") unit CommunityList.component unit
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
  HH.div_ []
