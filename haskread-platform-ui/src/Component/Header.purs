module Component.Header where

import Prelude

import Bulma.CSS.Spacing as B
import Bulma.Common as B
import Bulma.Components.Navbar as B
import Bulma.Elements.Button as Button
import Bulma.Form.Core as B
import Bulma.Modifiers.Helpers as B
import Bulma.Modifiers.Typography as B
import Capability.Resource (class Navigate, navigate)
import Utils.Bulma (class_,classes_)
import Common.Types (Profile, MyRoute(..))
import Common.Utils (defaultHomeOps, whenElem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..)) as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Store as Store
import Web.Event.Event (Event)
import Web.Event.Event as Event
-- import Web.HTML.Common (ClassName(..)) as HC

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
      GoToMyProfile -> navigate $ Home defaultHomeOps -- TODO: Add myProfile page and add here
      SetSearchInput searchInput ->
        H.modify_ _ { searchInput = searchInput }
      GoToHome -> do navigate $ Home defaultHomeOps
      HandleSearch event -> do
        H.liftEffect $ Event.preventDefault event
        -- _ <- H.get
        -- TODO: Add Search page and API
        navigate $ Home defaultHomeOps

  render :: State -> H.ComponentHTML Action () m
  render { currentUser, searchError, searchInput } = do
    HH.nav
      [ classes_
          [ B.navbar
          , B.isFixedTop
          , B.px6
          , B.hasShadow
          ]
      ]
      [ logoAndTitle
      , searchBar searchError searchInput
      , loginSignupButtons currentUser
      ]

  loginSignupButtons currUser =
    HH.div [ HP.id "navbarBasicExample", class_ B.navbarMenu ]
      [ HH.div [ class_ B.navbarEnd ]
          [ HH.div [ class_ B.navbarItem ]
              [ case currUser of
                  Nothing ->
                    HH.div [ class_ Button.buttons ]
                      [ HH.a
                          [ HE.onClick \_ -> GoToLogin
                          , classes_ [ Button.button, B.isPrimary ]
                          ]
                          [ HH.text "Login" ]
                      , HH.a
                          [ HE.onClick \_ -> GoToRegister
                          , classes_ [ Button.button, Button.isLight ]
                          ]
                          [ HH.text "Sign up" ]
                      ]
                  Just cUser ->
                    HH.div [ classes_ [ Button.button, B.isWarning ] ]
                      [ HH.a [ HE.onClick \_ -> GoToMyProfile, class_ B.px2 ]
                          [ HH.text cUser.userName ]
                      ]
              -- TODO: figure with image link
              ]
          ]
      ]

  navbarBurger = HH.a
    [ HP.attr (HC.AttrName "area-label") "Close"
    , class_ B.navbarBurger
    , HP.attr (HC.AttrName "area-label") "menu"
    , HP.attr (HC.AttrName "area-expanded") "false"
    , HP.attr (HC.AttrName "data-target") "navbarBasicExample"
    ]
    [ HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    , HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    , HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    , HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] []
    ]

  logoAndTitle = HH.div [ class_ B.navbarBrand ]
    [ HH.a [ HE.onClick \_ -> GoToHome, class_ B.navbarItem ]
        [ HH.p [ classes_ [ B.hasWeight B.Bold ] ] [ HH.text "HaskRead" ] ]
    , navbarBurger
    ]

  searchBar searchError searchInput = HH.div
    [ class_ B.navbarItem ]
    [ HH.form
        [ HE.onSubmit HandleSearch, classes_ [ B.field, B.hasAddons ] ]
        [ whenElem (isJust searchError)
            (\_ -> HH.div_ [ HH.text (fromMaybe "" searchError) ])
        , HH.div [ class_ B.control ]
            [ HH.input
                [ HP.placeholder "Search threads, community, users etc."
                , HP.type_ HP.InputText
                , HE.onValueInput SetSearchInput
                , HP.value searchInput
                , class_ B.input
                ]
            ]
        , HH.div [ class_ B.control ]
         [ HH.button
           [ HP.type_ HP.ButtonSubmit, classes_ [Button.button,B.isInfo] ]
            [ HH.i [HP.classes [HH.ClassName "bx bx-search bx-tada"],
                    HP.style "line-height:1.5"] [] ] ]
        ]
    ]
