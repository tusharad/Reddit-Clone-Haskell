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
import Common.Types (MyRoute(..), PaginatedArray, Profile, Thread, ThreadInfo, Pagination)
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
  { pagination_ :: Pagination
  }

type State =
  { threads :: RemoteData String (PaginatedArray ThreadInfo)
  , currentUser :: Maybe Profile
  , pagination_ :: Pagination
  }

data Action = Initialize | LoadThreads | GoToLogin | ChangePagination Int Int

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
                    [ tabCSS "Top Voted" "bx bxs-objects-vertical-top"
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
            , paginationView state.pagination_ 10
            ]
        , HH.slot_ (Proxy :: _ "communityList") unit CommunityList.component unit
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
      mThreadList <- getThreads s.pagination_
      case mThreadList of
        Nothing ->
          H.modify_ _ { threads = Failure "failed to load threads." }
        Just threadList -> do
          threadInfoList <- liftEffect $ toThreadInfo threadList
          H.modify_ _ { threads = fromMaybe (Just threadInfoList) }

    GoToLogin -> navigate Login
    ChangePagination limit offset -> do
        navigate $ Home { limit, offset } 
        _ <- H.modify_ _ { pagination_ = { limit, offset } } 
        void $ H.fork $ handleAction LoadThreads

paginationView :: Pagination -> Int -> forall props act. HH.HTML props Action
paginationView { offset } _ = do
  let nextOffset = offset + 10 --TODO: Need to find a when to disable next button.
  let prevOffset = if (offset - 10 > 0) then offset - 10 else 0
  HH.nav
    [ classes_ [ B.pagination, B.isRounded, B.pt4 ]
    , HP.attr (HC.AttrName "area-label") "pagination"
    , HP.attr (HC.AttrName "role") "navigation"
    ]
    [ HH.a [ class_ B.paginationPrevious, HE.onClick \_ -> ChangePagination 10 prevOffset ] [ HH.text "Previous" ]
    , HH.a [ class_ B.paginationNext, HE.onClick \_ -> ChangePagination 10 nextOffset ] [ HH.text "Next" ]
    , HH.ul [ class_ B.paginationList ]
        [ HH.li_ [ HH.a [ class_ B.paginationLink, HP.attr (HC.AttrName "area-label") "Go to page 1" ] [ HH.text "1" ] ]
        , HH.li_ [ HH.span [ class_ B.paginationEllipsis ] [ HH.text "..." ] ]
        , HH.li_ [ HH.a [ class_ B.paginationLink, HP.attr (HC.AttrName "area-label") "Go to page 45" ] [ HH.text "45" ] ]
        , HH.li_
            [ HH.a
                [ classes_ [ B.paginationLink, B.isCurrent ]
                , HP.attr (HC.AttrName "area-label") "Go to page 46"
                ]
                [ HH.text "46" ]
            ]
        , HH.li_ [ HH.a [ class_ B.paginationLink, HP.attr (HC.AttrName "area-label") "Go to page 1" ] [ HH.text "47" ] ]
        , HH.li_ [ HH.span [ class_ B.paginationEllipsis ] [ HH.text "..." ] ]
        , HH.li_ [ HH.a [ class_ B.paginationLink, HP.attr (HC.AttrName "area-label") "Go to page 45" ] [ HH.text "86" ] ]
        ]
    ]
