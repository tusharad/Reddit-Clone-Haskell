module Page.Login where

import Prelude

import Capability.Resource (class ManageUser, loginUser, class Navigate, navigate)
import Common.Types (MyRoute(..))
import Common.Utils (defaultPagination, safeHref, whenElem)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Input = { redirect :: Boolean }

data Action
  = SetEmail String
  | SetPassword String
  | HandleSubmit Event

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

  render :: State -> H.ComponentHTML Action () m
  render st =
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
