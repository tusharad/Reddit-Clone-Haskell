module Component.CommunityList where

import Prelude

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
import Network.RemoteData (RemoteData(..),fromMaybe)
import Store as Store

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
        HH.div_ [
            viewCommunityList communities
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
             HH.div_ (
                communityPreview `mapWithIndex` communityList.body
                )

    communityPreview :: forall props act. Int -> Community -> HH.HTML props act
    communityPreview _ community =
        HH.div_ [
            HH.text community.communityName 
        ]