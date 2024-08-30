module Page.Register where

import Prelude
import Undefined (undefined)
import Halogen as H
import Halogen.HTML as HH
import Formless as F
import Form.Validation (FormError)
import Form.Validation as V
import Capability.Navigate (class Navigate, navigate)
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Capability.Resource (class ManageUser, registerUser)
import Common.Types (MyRoute(..))
import Common.Utils (safeHref,whenElem)
import Halogen.HTML.Events as HE
import Form.Field as Field
import Halogen.HTML.Properties as HP
import Effect.Class.Console (log)
import Data.Either (Either(..))

type Input = { redirect :: Boolean }

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( 
    userNameForRegister :: f String FormError String,
    emailForRegister :: f String FormError String,
    passwordForRegister :: f String FormError String,
    confirmPasswordForRegister :: f String FormError String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type State =
  { form :: FormContext
  , registerError :: Boolean
  }

component :: 
  forall query output m. 
  MonadAff m =>
  Navigate m =>
  ManageUser m =>
  H.Component query Input output m
component =  F.formless { liftAction : Eval } mempty $ H.mkComponent {
        initialState : \context ->  { form: context, registerError : false } ,
        render,
        eval : H.mkEval H.defaultEval {
          receive = Just <<< Receive
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Receive context -> H.modify_ _ { form = context }
      Eval action -> F.eval action

    handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery = do 
      let
        onSubmit = registerUser >=> case _ of
          Left _ -> 
            H.modify_ _ { registerError = true }
          Right _ -> do
            H.modify_ _ { registerError = false }
            { redirect } <- H.gets _.form.input
            -- initiating OTP process
            log "dasdasdasdasdsa"
            when redirect (navigate $ OTP 1)

        validation =
          {
          passwordForRegister : V.required ,
          confirmPasswordForRegister : V.required ,
          emailForRegister : V.required,
          userNameForRegister : V.required
          }
      
      F.handleSubmitValidate onSubmit F.validate validation

    render :: State -> H.ComponentHTML Action () m
    render { registerError, form: { formActions, fields, actions } } =
      HH.div_
      [ HH.h1
          [  ]
          [ HH.text "Sign up" ]
      , HH.p
          [  ]
          [ HH.a
              [ safeHref Login ] -- 
              [ HH.text "Already have an account?" ]
          ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ whenElem registerError \_ ->
              HH.div
                [  ]
                [ HH.text "Something went wrong" ]
          , HH.fieldset_
              [
                Field.textInput
                  { state: fields.userNameForRegister, action: actions.userNameForRegister }
                  [ HP.placeholder "Username"
                  , HP.type_ HP.InputText
                  ]
                , Field.textInput
                  { state: fields.emailForRegister, action: actions.emailForRegister }
                  [ HP.placeholder "Email"
                  , HP.type_ HP.InputEmail
                  ]
              , Field.textInput
                  { state: fields.passwordForRegister, action: actions.passwordForRegister }
                  [ HP.placeholder "Password"
                  , HP.type_ HP.InputPassword
                  ]
              , 
              Field.textInput
                  { state: fields.confirmPasswordForRegister, action: actions.confirmPasswordForRegister }
                  [ HP.placeholder "Confirm password"
                  , HP.type_ HP.InputPassword
                  ]
              , Field.submitButton "Get OTP"
              ]
          ]
      ]
