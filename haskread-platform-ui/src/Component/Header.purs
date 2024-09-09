module Component.Header where

import Prelude

import Bulma.CSS.Spacing as B
import Bulma.Common as B
import Bulma.Components.Navbar as B
import Bulma.Form.General as B
import Bulma.Modifiers.Typography as B
import Bulma.Elements.Button as Button
import Bulma.Modifiers.Helpers as B
import Capability.Resource (class Navigate, navigate)
import Common.BulmaUtils as BU
import Common.Types (Profile, MyRoute(..))
import Common.Utils (defaultPagination, whenElem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Store as Store
import Undefined (undefined)
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State =
  { currentUser :: Maybe Profile
  , searchError :: Maybe String
  , searchInput :: String
  }

data Action
  = Initialize
  | GoToLogin
  | GoToRegister
  | HandleSearch Event
  | GoToMyProfile
  | SetSearchInput String
  | GoToHome

component
  :: forall query output m
   . MonadAff m
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
  initialState { context: currentUser } =
    { currentUser
    , searchError: Nothing
    , searchInput: ""
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction =
    case _ of
      Initialize -> do
        -- nothing here right now!
        pure unit
      GoToLogin -> navigate Login
      GoToRegister -> navigate Register
      GoToMyProfile -> navigate $ Home defaultPagination -- TODO: Add myProfile page and add here
      SetSearchInput searchInput ->
        H.modify_ _ { searchInput = searchInput }
      GoToHome -> navigate $ Home defaultPagination
      HandleSearch event -> do
        H.liftEffect $ Event.preventDefault event
        -- _ <- H.get
        -- TODO: Add Search page and API
        navigate $ Home defaultPagination

  render :: State -> H.ComponentHTML Action () m
  render { currentUser, searchError, searchInput } = do
    HH.nav
      [ BU.classNames
          [ B.navbar
          , B.isFixedTop
          , B.px6
          , B.hasShadow
          ]
      ]
      [ logoAndTitle
      , searchBar searchError searchInput
      , loginSignupButtons currentUser
      --   case currentUser of
      --       Nothing -> loginRegisterButtons
      --       Just currUser -> profileButton currUser
      ]

  loginSignupButtons currUser =
    HH.div [ HP.id "navbarBasicExample", BU.className B.navbarMenu ]
      [ HH.div [ BU.className B.navbarEnd ]
          [ HH.div [ BU.className B.navbarItem ]
              [ HH.div [ BU.className Button.buttons ]
                  [ HH.a
                      [ HE.onClick \_ -> GoToLogin
                      , BU.classNames [ Button.button, B.isPrimary ]
                      ]
                      [ HH.text "Login" ]
                  ]
              ]
          ]
      ]

  navbarBurger = HH.a
    [ HP.attr (HC.AttrName "area-label") "Close"
    , BU.className B.navbarBurger
    , HP.attr (HC.AttrName "area-label") "menu"
    , HP.attr (HC.AttrName "area-expanded") "false"
    , HP.attr (HC.AttrName "data-target") "navbarBasicExample"
    ]
    [ HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    , HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    , HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    , HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    ]

  logoAndTitle = HH.div [ BU.className B.navbarBrand ]
    [ HH.a [ HE.onClick \_ -> GoToHome, BU.className B.navbarItem ]
        [ HH.p [ BU.classNames [ B.hasWeight B.Bold ] ] [ HH.text "HaskRead" ] ]
    , navbarBurger
    ]

  searchBar searchError searchInput = HH.div
    [ BU.className B.navbarItem ]
    [ HH.div [ BU.classNames [ B.field, B.hasAddons ] ]
        [ HH.form
            [ HE.onSubmit HandleSearch ]
            [ whenElem (isJust searchError)
                (\_ -> HH.div_ [ HH.text (fromMaybe "" searchError) ])
            , HH.div [ BU.className B.control ]
                [ HH.input
                    [ HP.placeholder "Search threads, community, users etc."
                    , HP.type_ HP.InputText
                    , HE.onValueInput SetSearchInput
                    , HP.value searchInput
                    ]
                ]
            , HH.div [ BU.className B.control ] [ HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Search" ] ]
            ]
        ]
    ]

  loginRegisterButtons = HH.div_
    [ HH.button [ HE.onClick \_ -> GoToLogin ] [ HH.text "Login" ]
    , HH.button [ HE.onClick \_ -> GoToRegister ] [ HH.text "Register" ]
    ]

  profileButton currUser = HH.div_
    [ HH.button [ HE.onClick \_ -> GoToMyProfile ] [ HH.text currUser.userName ]
    ]
