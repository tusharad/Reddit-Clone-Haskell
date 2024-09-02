module Page.OTP where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Formless as F
import Form.Validation (FormError)
import Form.Validation as V
import Capability.Navigate (class Navigate, navigate)
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Capability.Resource (class ManageUser, verifyOtp)
import Common.Types (MyRoute(..))
import Common.Utils (safeHref,whenElem)
import Halogen.HTML.Events as HE
import Form.Field as Field
import Halogen.HTML.Properties as HP
import Data.Either (Either(..))
import Data.Int (fromString)
import Store as Store
import Halogen.Store.Monad (class MonadStore)

type Input = {uId :: Int }

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( 
    otpField :: f String FormError String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type State =
  { form :: FormContext
  , otpError :: Boolean
  , userID :: Int
  }

toInt :: String ->  Int
toInt str = 
    case fromString str of
        Nothing -> 23
        Just num -> num

component :: 
  forall query output m. 
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  ManageUser m =>
  H.Component query Input output m
component =  F.formless { liftAction : Eval } mempty $ H.mkComponent {
        initialState,
        render,
        eval : H.mkEval H.defaultEval {
          receive = Just <<< Receive
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
    initialState ctx = { form: ctx, otpError : false, userID : ctx.input.uId} 

    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Receive context -> H.modify_ _ { form = context }
      Eval action -> F.eval action

    handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery r = do
      let
        onSubmit = do
          verifyOtp >=> case _ of
            Left _ -> do
               H.modify_ _ { otpError = true }
            Right _ -> do
               H.modify_ _ { otpError = false }
               -- initiating OTP process
               navigate Home

        validation =
          {
          otpField : V.required
          }
      
      { userID } <- H.get
      let onSubmit_ { otpField} = 
            onSubmit { otp : toInt otpField,userID : userID}
      F.handleSubmitValidate onSubmit_ F.validate validation r

    render :: State -> H.ComponentHTML Action () m
    render { otpError, form: { formActions, fields, actions} } =
      HH.div_
      [ HH.h1
          [  ]
          [ HH.text "OTP Veify" ]
      , HH.p
          [  ]
          [ HH.a
              [ safeHref Register] -- 
              [ HH.text "wanna Resend?" ]
          ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ whenElem otpError \_ ->
              HH.div
                [  ]
                [ HH.text "Something went wrong" ]
          , HH.fieldset_
              [
                Field.textInput
                  { state: fields.otpField , action: actions.otpField}
                  [ HP.placeholder "Enter OTP"
                  , HP.type_ HP.InputNumber
                  ]
                , Field.submitButton "Verify"
              ]
          ]
      ]
