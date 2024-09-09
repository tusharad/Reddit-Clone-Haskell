module Component.Header where

import Prelude
import Data.Maybe (Maybe(..),fromMaybe,isJust)
import Common.Types (Profile,MyRoute(..))
import Halogen.Store.Monad (class MonadStore)
import Capability.Resource (class Navigate,navigate)
import Effect.Aff.Class (class MonadAff)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectEq)
import Store as Store
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Common.Utils (whenElem)

-- Bulma
import Bulma.Components.Navbar as B
import Bulma.Common as B
import Common.BulmaUtils as BU
import Bulma.Layout.Layout as B
import Bulma.CSS.Spacing as B

type State = {
        currentUser :: Maybe Profile
      , searchError :: Maybe String
      , searchInput :: String
    }

data Action = Initialize
            | GoToLogin
            | GoToRegister
            | HandleSearch Event
            | GoToMyProfile
            | SetSearchInput String

component 
    :: forall query output m
    .MonadAff m
    => Navigate m
    => MonadStore Store.Action Store.Store m
    => H.Component query Unit output m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }
  where
    initialState { context : currentUser } = { 
              currentUser
            , searchError : Nothing 
            , searchInput : ""
        }

    handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
    handleAction = 
        case _ of
            Initialize -> do
               -- nothing here right now!
               pure unit
            GoToLogin -> navigate Login
            GoToRegister -> navigate Register
            GoToMyProfile -> navigate Home -- TODO: Add myProfile page and add here
            SetSearchInput searchInput -> 
                H.modify_ _ { searchInput = searchInput }
            HandleSearch event -> do
               H.liftEffect $ Event.preventDefault event
               { searchInput } <- H.get
               -- TODO: Add Search page and API
               navigate Home

    render :: State -> H.ComponentHTML Action () m
    render { currentUser,searchError,searchInput } = do
        HH.nav [
            BU.classNames [
                B.navbar,
                B.isFixedTop,
                B.px6
            ]
        ] [
            logoAndTitle,
            searchBar searchError searchInput,
            case currentUser of
                Nothing -> loginRegisterButtons
                Just currUser -> profileButton currUser
        ]

    logoAndTitle = HH.div_ [HH.h3_ [ HH.text "HaskRead" ]]

    searchBar searchError searchInput = HH.div_ [
        HH.form 
            [ HE.onSubmit HandleSearch ]
            [ whenElem (isJust searchError)
                    (\_ -> HH.div_ [ HH.text (fromMaybe "" searchError) ])
            , HH.input [
                 HP.placeholder "Search threads, community, users etc."
                  , HP.type_ HP.InputText
                  , HE.onValueInput SetSearchInput
                  , HP.value searchInput ] 
             , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Search" ]     
            ]
    ]

    loginRegisterButtons = HH.div_ [
        HH.button [ HE.onClick \_ -> GoToLogin ] [HH.text "Login"]
      , HH.button [ HE.onClick \_ -> GoToRegister ] [HH.text "Register"]
    ]

    profileButton currUser = HH.div_ [
        HH.button [ HE.onClick \_ -> GoToMyProfile ] [HH.text currUser.userName]
    ]
