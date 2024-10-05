module Page.OTP where

import Prelude

import Capability.Resource (class ManageUser, verifyOtp, class Navigate, navigate)
import Common.Types (MyRoute(..))
import Common.Utils (defaultHomeOps, safeHref, whenElem)
import Data.Either (Either(..), isLeft, hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.String (length)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Input = { userID :: Int }

data Action
  = SetOTP String
  | HandleSubmit Event

type State =
  { otp :: String
  , userID :: Int
  , otpError :: Either String Unit
  }

validateInput :: State -> Either String Unit
validateInput { otp } = do
  if (length otp /= 4) then Left "OTP should be 4 digit long!"
  else if (isNothing (fromString otp)) then Left "OTP should be digits only"
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
  initialState { userID } =
    { otp: ""
    , userID: userID
    , otpError: Right unit
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    SetOTP otp -> H.modify_ _ { otp = otp }
    HandleSubmit event -> do
      H.liftEffect $ Event.preventDefault event
      st <- H.get
      mRes <- case validateInput st of
        Left err -> do
          H.modify_ _ { otpError = Left err }
          log err *> pure Nothing
        Right _ -> do
          let
            otpFields =
              { otp: fromMaybe (-1) (fromString st.otp)
              , userID: st.userID
              }
          res <- verifyOtp otpFields
          pure $ hush res
      case mRes of
        Nothing -> pure unit
        Just _ -> navigate (Home defaultHomeOps)

  render :: State -> H.ComponentHTML Action () m
  render st =
    HH.div_
      [ HH.h1
          []
          [ HH.text "Enter OTP" ]
      , HH.p
          []
          [ HH.a
              [ safeHref Register ] -- 
              [ HH.text "Wanna resend?" ]
          ]
      , HH.form
          [ HE.onSubmit HandleSubmit ]
          [ whenElem (isLeft st.otpError) \_ ->
              HH.div
                []
                [ HH.text "Something went wrong!" ]
          , HH.fieldset_
              [ HH.input
                  [ HP.placeholder "Enter OTP e.g 1234"
                  , HP.type_ HP.InputNumber
                  , HE.onValueInput SetOTP
                  , HP.value st.otp
                  ]
              , HH.button
                  [ HP.type_ HP.ButtonSubmit ]
                  [ HH.text "Verify" ]
              ]
          ]
      ]
