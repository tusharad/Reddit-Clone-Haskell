module Page.Home where

import Prelude

import Bulma.CSS.Spacing as B
import Bulma.Columns.Columns as B
import Bulma.Columns.Size hiding (isSmall) as B
import Bulma.Components.Pagination as B
import Bulma.Components.Tabs as B
import Bulma.Elements.Button as B
import Bulma.Elements.Elements as B
import Bulma.Form.Common as B
import Bulma.Form.General as B
import Bulma.Layout.Layout as B
import Bulma.Modifiers.Typography as B
import Capability.Resource (class ManageCommunity, class ManageThreads, class Navigate, getThreads, navigate)
import Common.Types (MyRoute(..), PaginatedArray, Profile, Thread, ThreadInfo, Pagination, HomeOps)
import Common.Utils (safeHref, stringToDate, toThreadInfo)
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
import Effect.Class.Console (log)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..)) as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Type.Proxy (Proxy(..))
import Utils.Bulma (class_, classes_)

type Input =
  { homeOps :: HomeOps
  }

type State =
  { threads :: RemoteData String (PaginatedArray ThreadInfo)
  , currentUser :: Maybe Profile
  , homeOps :: HomeOps
  }

data Action = Initialize 
    | LoadThreads | GoToLogin | ChangePagination Int Int | HandleCommunityList CommunityList.Output

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = (
        header :: OpaqueSlot Unit
        , footer :: OpaqueSlot Unit
        , communityList :: forall query. H.Slot query CommunityList.Output Unit
        , threadView :: OpaqueSlot Unit)

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
  initialState { context: currentUser, input: { homeOps } } =
    { threads: NotAsked, currentUser, homeOps }

  tabCSS textVal iconVal = HH.li [ class_ B.isActive ]
    [ HH.a_
        [ HH.span [ classes_ [ B.icon, B.isSmall ] ]
            [ HH.i [ HP.class_ $ HH.ClassName iconVal ] [] ]
        , HH.span_ [ HH.text textVal ]
        ]
    ]

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div [ class_ B.container ]
    [ HH.slot_ (Proxy :: _ "header") unit Header.component unit
    , HH.div [ classes_ [ B.columns, B.is8, B.py5 ] ]
        [ HH.div [ class_ B.column ]
            [ HH.p [ classes_ [ B.isSize3, B.hasTextCentered, B.pb4 ] ] [ HH.text "Threads" ]
            , HH.div [ classes_ [ B.tabs, B.isCentered, B.isRounded ] ]
                [ HH.ul_
                    [ 
                      HH.div [HP.class_ $ HH.ClassName "dropdown "] [
                        HH.div [HP.class_ $ HH.ClassName "dropdown-trigger"] [
                            HH.button [ HP.attr (HC.AttrName "area-haspopup") "true"
                                    , HP.attr (HC.AttrName "area-controls") "dropdown-menu"
                                        ] [HH.span_ [HH.text "Top"]]
                        , HH.div [HP.class_ $ HH.ClassName "dropdown-menu"
                                 , HP.id "dropdown-menu"
                                 , HP.attr (HC.AttrName "role")"menu"] 
                                 [
                                    HH.div [ HP.class_ $ HH.ClassName "dropdown-content" ] [
                                        HH.a [HP.class_ $ HH.ClassName "dropdown-item"] [ HH.text "top of day" ]
                                    ]
                                ]
                        ]
                      ]
                                                   -- tabCSS "Top Voted" "bx bxs-objects-vertical-top"
                    , tabCSS "Trending" "bx bx-trending-up"
                    , tabCSS "New" "bx bx-polygon"
                    , tabCSS "Following" "bx bx-run"
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
            , paginationView state.homeOps 10
            ]
        , HH.slot (Proxy :: _ "communityList") unit CommunityList.component unit HandleCommunityList 
        ]
    , HH.slot_ (Proxy :: _ "footer") unit Footer.component unit
    ]

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadThreads

    LoadThreads -> do
      _ <- H.modify_ _ { threads = Loading }
      s <- H.get
      mThreadList <- getThreads s.homeOps
      case mThreadList of
        Nothing ->
          H.modify_ _ { threads = Failure "failed to load threads." }
        Just threadList -> do
          threadInfoList <- liftEffect $ toThreadInfo threadList
          H.modify_ _ { threads = fromMaybe (Just threadInfoList) }

    GoToLogin -> navigate Login
    
    ChangePagination limit offset -> do
        { homeOps : h } <- H.get
        navigate $ Home { limit, offset, communityId : h.communityId, sortBy: Nothing } 
        _ <- H.modify_ _ { homeOps = { limit, offset, communityId: h.communityId, sortBy: Nothing } } 
        void $ H.fork $ handleAction LoadThreads
    
    HandleCommunityList op -> do
        case op of
            CommunityList.Clicked n -> do
               H.modify_ _ { homeOps = { offset: 0, limit: 10, communityId: Just n, sortBy: Nothing } }
               void $ H.fork $ handleAction LoadThreads

paginationView :: HomeOps -> Int -> forall props act. HH.HTML props Action
paginationView { offset, communityId } _ = do
  -- let nextOffset = offset + 10 --TODO: Need to find a when to disable next button.
  -- let prevOffset = if (offset - 10 > 0) then offset - 10 else 0
  HH.nav
    [ classes_ [ B.pagination, B.isRounded, B.pt4 ]
    , HP.attr (HC.AttrName "area-label") "pagination"
    , HP.attr (HC.AttrName "role") "navigation"
    ]
    [ HH.a 
        [ class_ B.paginationPrevious
          , HE.onClick \_ -> ChangePagination 10 (offset-10) ] 
        [ HH.text "Previous" ]
    , HH.a 
        [ class_ B.paginationNext
        , HE.onClick \_ -> ChangePagination 10 (offset+10) ] 
        [ HH.text "Next" ]
    ]
