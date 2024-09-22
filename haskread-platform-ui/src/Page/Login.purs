module Page.Login where

import Prelude

import Bulma.CSS.Spacing (pt4, py6)
import Bulma.Columns (column, columns, isHalf)
import Bulma.Common (isFullwidth)
import Bulma.Elements.Button (button)
import Bulma.Elements.Elements (box)
import Bulma.Form.General (IconAlignment(..)) as Bulma
import Bulma.Form.General (IconAlignment, control, field, hasIconAlignment, isCentered, label)
import Bulma.Form.Input (input)
import Bulma.Modifiers.Helpers (isPrimary)
import Bulma.Modifiers.Typography (hasTextCentered, hasTextWeightSemiBold, isSize4)
import Capability.Resource (class ManageUser, class Navigate, loginUser, navigate)
import Common.Types (MyRoute(..))
import Common.Utils (defaultPagination, safeHref, whenElem)
import Component.Footer as Footer
import Component.Header as Header
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Store as Store
import Type.Proxy (Proxy(..))
import Utils.Bulma (class_, classes_)
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Input = { redirect :: Boolean }

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = (header :: OpaqueSlot Unit, footer :: OpaqueSlot Unit, communityList :: OpaqueSlot Unit, threadView :: OpaqueSlot Unit)

data Action
  = SetEmail String
  | SetPassword String
  | HandleSubmit Event
  | GoToSignup

type State =
  { email :: String
  , password :: String
  , loginError :: Either String Unit
  }

validateInput :: State -> Either String Unit
validateInput { email, password } = do
  if (email == mempty) then Left "email required"
  else if (password == mempty) then Left "password required"
  else (Right unit)

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
  }
  where
  initialState _ =
    { email: ""
    , password: ""
    , loginError: Right unit
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    SetEmail email -> H.modify_ _ { email = email }
    SetPassword password -> H.modify_ _ { password = password }
    GoToSignup -> navigate Register
    HandleSubmit event -> do
      H.liftEffect $ Event.preventDefault event
      st <- H.get
      mRes <- case validateInput st of
        Left err -> do
          H.modify_ _ { loginError = Left err }
          log err *> pure Nothing
        Right _ -> do
          let
            loginFields =
              { emailForLogin: st.email
              , passwordForLogin: st.password
              }
          loginUser loginFields
      case mRes of
        Nothing -> pure unit
        Just _ -> navigate (Home defaultPagination)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    HH.div_
      [ HH.slot_ (Proxy :: _ "header") unit Header.component unit
      , HH.div [ classes_ [ columns, isCentered, py6 ] ]
          [ HH.div [ classes_ [ column, isHalf ] ]
              [ HH.div [ class_ box ]
                  [ HH.p [ classes_ [ isSize4, hasTextCentered, hasTextWeightSemiBold ] ] [ HH.text "Login to Haskread" ]
                  , HH.form [ HE.onSubmit HandleSubmit ]
                      [ whenElem (isLeft st.loginError) \_ ->
                          HH.div
                            []
                            [ HH.text "Email or password is invalid" ]
                      , HH.div [ class_ field ]
                          [ HH.label [ class_ label ] [ HH.text "Email" ]
                          , HH.div [ classes_ [ control, hasIconAlignment Bulma.IconLeft ] ]
                              [ HH.input
                                  [ class_ input
                                  , HP.type_ HP.InputEmail
                                  , HP.placeholder "Enter your email"
                                  , HE.onValueInput SetEmail
                                  , HP.value st.email
                                  ]
                              , HH.span [ HP.class_ $ HH.ClassName "icon is-small is-left" ]
                                  [ HH.i [ HP.class_ $ HH.ClassName "bx bx-envelope" ] []
                                  ]
                              ]
                          ]
                      , HH.div [ class_ field ]
                          [ HH.label [ class_ label ] [ HH.text "Password" ]
                          , HH.div [ classes_ [ control, hasIconAlignment Bulma.IconLeft ] ]
                              [ HH.input
                                  [ class_ input
                                  , HP.type_ HP.InputPassword
                                  , HP.placeholder "Enter your password"
                                  , HE.onValueInput SetPassword
                                  , HP.value st.password
                                  ]
                              , HH.span [ HP.class_ $ HH.ClassName "icon is-small is-left" ]
                                  [ HH.i [ HP.class_ $ HH.ClassName "bx bx-lock" ] []
                                  ]
                              ]
                          ]
                      , HH.div [ class_ field ]
                          [ HH.div [ class_ control ]
                              [ HH.button [ classes_ [ button, isPrimary, isFullwidth ] ]
                                  [ HH.strong_ [ HH.text "Login" ]
                                  ]
                              ]
                          ]
                      ]
                  , HH.p [ classes_ [ hasTextCentered, pt4 ] ] [ HH.text "Don't have an account? ", HH.a [ onClick \_ -> GoToSignup ] [ HH.text "sign up" ] ]
                  ]
              ]
          ]
      , HH.slot_ (Proxy :: _ "footer") unit Footer.component unit
      ]
{-
    HH.div_
      [ HH.h1
          []
          [ HH.text "Sign In" ]
      , HH.p
          []
          [ HH.a
              [ safeHref Register ] -- 
              [ HH.text "Need an account?" ]
          ]
      , HH.form
          [ HE.onSubmit HandleSubmit ]
          [ whenElem (isLeft st.loginError) \_ ->
              HH.div
                []
                [ HH.text "Email or password is invalid" ]
          , HH.fieldset_
              [ HH.input
                  [ HP.placeholder "Email"
                  , HP.type_ HP.InputEmail
                  , HE.onValueInput SetEmail
                  , HP.value st.email
                  ]
              , HH.input
                  [ HP.placeholder "Password"
                  , HP.type_ HP.InputPassword
                  , HE.onValueInput SetPassword
                  , HP.value st.password
                  ]
              , HH.button
                  [ HP.type_ HP.ButtonSubmit ]
                  [ HH.text "Login" ]
              ]
          ]
      ]

-}