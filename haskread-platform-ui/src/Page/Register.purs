module Page.Register where

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
import Capability.Resource (class ManageUser, class Navigate, navigate, registerUser)
import Common.Types (MyRoute(..))
import Common.Utils (safeHref, whenElem)
import Component.Footer as Footer
import Component.Header as Header
import Data.Either (Either(..), isLeft)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
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
  = SetUserName String
  | SetEmail String
  | SetPassword String
  | SetConfirmPassword String
  | HandleSubmit Event
  | GoToLogin

type State =
  { userName :: String
  , email :: String
  , password :: String
  , confirmPassword :: String
  , registerError :: Either String Unit
  }

validateInput :: State -> Either String Unit
validateInput { email, password, confirmPassword, userName } = do
  if (email == mempty) then Left "email required"
  else if (password == mempty) then Left "password required"
  else if (confirmPassword == mempty) then Left "confirm password required"
  else if (userName == mempty) then Left "userName required"
  else if (password /= confirmPassword) then Left "Passwords did not matched!"
  else (Right unit)

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => ManageUser m
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
    { userName: ""
    , email: ""
    , password: ""
    , confirmPassword: ""
    , registerError: Right unit
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    GoToLogin -> navigate Login
    SetUserName userName -> H.modify_ _ { userName = userName }
    SetEmail email -> H.modify_ _ { email = email }
    SetPassword password -> H.modify_ _ { password = password }
    SetConfirmPassword confirmPassword ->
      H.modify_ _ { confirmPassword = confirmPassword }
    HandleSubmit event -> do
      H.liftEffect $ Event.preventDefault event
      st <- H.get
      case validateInput st of
        Left err -> do
          H.modify_ _ { registerError = Left err }
          log err
        Right _ -> do
          let
            registerFields =
              { userNameForRegister: st.userName
              , emailForRegister: st.email
              , passwordForRegister: st.password
              , confirmPasswordForRegister: st.confirmPassword
              }
          eRes <- registerUser registerFields
          case eRes of
            Left _ -> log "Registration failed" *> pure unit
            Right userID -> navigate (OTP userID)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    HH.div_
      [ HH.slot_ (Proxy :: _ "header") unit Header.component unit
      , HH.div [ classes_ [ columns, isCentered, py6 ] ]
          [ HH.div [ classes_ [ column, isHalf ] ]
              [ HH.div [ class_ box ]
                  [ HH.p [ classes_ [ isSize4, hasTextCentered, hasTextWeightSemiBold ] ] [ HH.text "Signup to Haskread" ]
                  , HH.form [ HE.onSubmit HandleSubmit ]
                      [ whenElem (isLeft st.registerError) \_ ->
                          HH.div
                            []
                            [ HH.text "Email or password is invalid" ]
                      , HH.div [ class_ field ]
                          [ HH.label [ class_ label ] [ HH.text "Username" ]
                          , HH.div [ classes_ [ control, hasIconAlignment Bulma.IconLeft ] ]
                              [ HH.input
                                  [ class_ input
                                  , HP.type_ HP.InputText
                                  , HP.placeholder "Enter username"
                                  , HE.onValueInput SetUserName
                                  , HP.value st.userName
                                  ]
                              , HH.span [ HP.class_ $ HH.ClassName "icon is-small is-left" ]
                                  [ HH.i [ HP.class_ $ HH.ClassName "" ] [] --TODO: add icon
                                  ]
                              ]
                          ]
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
                          [ HH.label [ class_ label ] [ HH.text "Confirm Password" ]
                          , HH.div [ classes_ [ control, hasIconAlignment Bulma.IconLeft ] ]
                              [ HH.input
                                  [ class_ input
                                  , HP.type_ HP.InputPassword
                                  , HP.placeholder "Enter your password"
                                  , HE.onValueInput SetConfirmPassword
                                  , HP.value st.confirmPassword
                                  ]
                              , HH.span [ HP.class_ $ HH.ClassName "icon is-small is-left" ]
                                  [ HH.i [ HP.class_ $ HH.ClassName "bx bx-lock" ] []
                                  ]
                              ]
                          ]
                      , HH.div [ class_ field ]
                          [ HH.div [ class_ control ]
                              [ HH.button [ classes_ [ button, isPrimary, isFullwidth ] ]
                                  [ HH.strong_ [ HH.text "Send OTP" ]
                                  ]
                              ]
                          ]
                      ]
                  , HH.p [ classes_ [ hasTextCentered, pt4 ] ] [ HH.text "Already have an account? ", HH.a [ onClick \_ -> GoToLogin ] [ HH.text "Login" ] ]
                  ]
              ]
          ]
      , HH.slot_ (Proxy :: _ "footer") unit Footer.component unit
      ]