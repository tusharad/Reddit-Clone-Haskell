module Page.DeleteUser where

import Prelude

import Capability.Resource (class ManageUser, deleteUser, class Navigate, navigate)
import Common.Types (MyRoute(..))
import Common.Utils (defaultPagination, whenElem)
import Data.Either (Either(..), isLeft, fromLeft)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State =
  { password :: String
  , areUSure :: Boolean
  , deleteUserError :: Either String Unit
  }

data Action
  = Initialize
  | HandleSubmit
  | SetPassword String
  | SetAreUSure Boolean
  | MakeRequest Event

validateInput :: State -> Either String Unit
validateInput { password } = do
  if (password == mempty) then Left "password is required"
  else (Right unit)

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageUser m
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
  initialState _ =
    { password: ""
    , areUSure: false
    , deleteUserError: Right unit
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> pure unit
    HandleSubmit -> pure unit
    SetPassword pass ->
      H.modify_ _ { password = pass }
    SetAreUSure res ->
      H.modify_ _ { areUSure = res }
    MakeRequest event -> do
      H.liftEffect $ Event.preventDefault event
      st <- H.get
      mRes <- case validateInput st of
        Left err -> do
          H.modify_ _ { deleteUserError = Left err }
          log err *> pure Nothing
        Right _ -> do
          let
            deleteUserFields =
              { passwordForDeleteUser: st.password
              , areUSure: st.areUSure
              }
          deleteUser deleteUserFields
      case mRes of
        Nothing -> pure unit
        Just _ -> navigate (Home defaultPagination)

  render :: State -> H.ComponentHTML Action _ m
  render st =
    HH.div_
      [ HH.form
          [ HE.onSubmit MakeRequest ]
          [ whenElem (isLeft st.deleteUserError) \_ ->
              HH.div
                []
                [ HH.text $ fromLeft "" st.deleteUserError ]

          , HH.label [] [ HH.text "Enter Password" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput SetPassword
              , HP.value st.password
              ]
          , HH.label [] [ HH.text "Are you sure?" ]
          , HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked st.areUSure
              , HE.onChecked SetAreUSure
              ]
          , HH.button
              [ HP.type_ HP.ButtonSubmit ]
              [ HH.text "submit" ]
          ]
      ]
