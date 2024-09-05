module Page.ChangePassword where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Capability.Resource (class ManageUser, changePassword, class Navigate, navigate)
import Common.Types (MyRoute(..))
import Common.Utils (whenElem)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Class.Console (log)
import Data.Either (Either(..), isLeft, fromLeft)
import Web.Event.Event as Event
import Web.Event.Event (Event)

type State =
  { currentPassword :: String
  , newPassword :: String
  , confirmNewPassword :: String
  , changePasswordError :: Either String Unit
  }

data Action
  = Initialize
  | HandleSubmit
  | SetCurrentPassword String
  | SetNewPassword String
  | SetConfirmNewPassword String
  | MakeRequest Event

validateInput :: State -> Either String Unit
validateInput { currentPassword, newPassword, confirmNewPassword } = do
  if (currentPassword == mempty) then Left "currentPassword required"
  else if (newPassword == mempty) then Left "newpassword required"
  else if (confirmNewPassword == mempty) then Left "confirmed new password required"
  else if (newPassword /= confirmNewPassword) then (Left "Passwords did not matched!")
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
    { currentPassword: ""
    , newPassword: ""
    , confirmNewPassword: ""
    , changePasswordError: Right unit
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> pure unit
    HandleSubmit -> pure unit
    SetCurrentPassword pass ->
      H.modify_ (_ { currentPassword = pass })
    SetNewPassword pass ->
      H.modify_ _ { newPassword = pass }
    SetConfirmNewPassword pass ->
      H.modify_ _ { confirmNewPassword = pass }
    MakeRequest event -> do
      H.liftEffect $ Event.preventDefault event
      st <- H.get
      mRes <- case validateInput st of
        Left err -> do
          H.modify_ _ { changePasswordError = Left err }
          log err *> pure Nothing
        Right _ -> do
          let
            changePasswordFields =
              { oldPasswordForChangePass: st.currentPassword
              , newPasswordForChangePass: st.newPassword
              , confirmPasswordForChangePass: st.confirmNewPassword
              }
          changePassword changePasswordFields
      case mRes of
        Nothing -> pure unit
        Just _ -> navigate Home

  render :: State -> H.ComponentHTML Action _ m
  render st =
    HH.div_
      [ HH.form
          [ HE.onSubmit MakeRequest ]
          [ whenElem (isLeft st.changePasswordError) \_ ->
              HH.div
                []
                [ HH.text $ fromLeft "" st.changePasswordError ]

          , HH.label [] [ HH.text "Enter Current Password" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput SetCurrentPassword
              , HP.value st.currentPassword
              ]
          , HH.label [] [ HH.text "Enter New Password" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput SetNewPassword
              , HP.value st.newPassword
              ]
          , HH.label [] [ HH.text "Confirm New Password" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput SetConfirmNewPassword
              , HP.value st.confirmNewPassword
              ]
          , HH.button
              [ HP.type_ HP.ButtonSubmit ]
              [ HH.text "submit" ]
          ]
      ]
