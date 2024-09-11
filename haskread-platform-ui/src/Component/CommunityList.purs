module Component.CommunityList where

import Prelude

import Bulma.CSS.Spacing as B
import Bulma.Columns.Columns as B
import Bulma.Columns.Size as B
import Bulma.Components.Card as B
import Bulma.Components.Menu as B
import Bulma.Components.Navbar as B
import Bulma.Components.Pagination as B
import Bulma.Components.Tabs as B
import Bulma.Elements.Button as B
import Bulma.Elements.Elements as B
import Bulma.Form.Common as B
import Bulma.Form.General as B
import Bulma.Modifiers.Typography as B
import Capability.Resource (class ManageCommunity, class Navigate, getCommunities, navigate)
import Common.Types (MyRoute(..), Community, PaginatedArray)
import Common.Utils (whenElem)
import Data.Array (mapWithIndex)
import Data.Foldable (length)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Utils.Bulma (class_, classes_)

type State = {
      communityError :: Maybe String,
      communities :: RemoteData String (PaginatedArray Community)
    }

data Action = Initialize | LoadCommunities

component 
    :: forall query output m
    .MonadAff m
    => Navigate m
    => ManageCommunity m
    => MonadStore Store.Action Store.Store m
    => H.Component query Unit output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }
  where
    initialState _ = { 
            communityError : Nothing,
            communities : NotAsked
        }

    handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
    handleAction = 
        case _ of
            Initialize -> do
               -- nothing here right now!
               void $ H.fork $ handleAction LoadCommunities
               pure unit
            LoadCommunities -> do
                _ <- H.modify_ _ { communities = Loading }
                mCommunityList <- getCommunities
                case mCommunityList of
                    Nothing -> H.modify_ _ { communities = Failure "asdas" }
                    Just communityList -> do
                        H.modify_ _ { communities = fromMaybe (Just communityList) }
    
    render :: State -> H.ComponentHTML Action () m
    render { communities, communityError } = do
        HH.div [ classes_ [ B.column, B.py5, B.isOneQuarter ] ] [
            -- viewCommunityList communities
            HH.p [classes_ [B.isSize3, B.hasTextCentered]] [HH.text "Trending Communities"]
          , HH.div [classes_ [B.card,B.my3]] [
            HH.header [class_ B.cardHeader] [
              HH.p [class_ B.cardHeaderTitle] [HH.text "Communites"]
            ]
          , HH.div [class_ B.cardContent] [
              HH.aside [class_ B.menu] []
             , viewCommunityList communities
            ]
          ]
        ]
    viewCommunityList :: forall props act
        . RemoteData String (PaginatedArray Community)
        -> HH.HTML props act
    viewCommunityList = case _ of
        NotAsked ->
          HH.text "Communities not loaded yet..."
        Loading ->
          HH.text "Loading..."
        Failure err ->
          HH.text ("Error loading communites: " <> err)
        Success { body } | length body == 0 ->
          HH.text "No communities are here...yet!"
        Success communityList -> do
             HH.ul [class_ B.menuList] (
                communityPreview `mapWithIndex` communityList.body
                )

    communityPreview :: forall props act. Int -> Community -> HH.HTML props act
    communityPreview _ community =
        HH.div_ [
          HH.li_ [HH.a [] [HH.text community.communityName]]
        , HH.hr [class_ B.navbarDivider, HP.style "border-bottom: solid;"]
        ]