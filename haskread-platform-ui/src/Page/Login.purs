module Page.Login
  where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Formless as F
import Form.Validation (FormError)
import Form.Validation as V
import Capability.Navigate (class Navigate, navigate)
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Capability.Resource (class ManageUser, loginUser)
import Common.Types (MyRoute(..))
import Common.Utils (safeHref,whenElem)
import Halogen.HTML.Events as HE
import Form.Field as Field
import Halogen.HTML.Properties as HP

type Input = { redirect :: Boolean }

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( emailForLogin :: f String FormError String
  , passwordForLogin :: f String FormError String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type State =
  { form :: FormContext
  , loginError :: Boolean
  }

component :: 
  forall query output m. 
  MonadAff m =>
  Navigate m =>
  ManageUser m =>
  H.Component query Input output m
component =  F.formless { liftAction : Eval } mempty $ H.mkComponent {
        initialState : \context ->  { form: context, loginError : false } ,
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
        onSubmit = loginUser >=> case _ of
          Nothing -> 
            H.modify_ _ { loginError = true }
          Just _ -> do
            H.modify_ _ { loginError = false }
            { redirect } <- H.gets _.form.input
            when redirect (navigate Home)

        validation =
          {
            emailForLogin : V.required >=> V.minLength 3 >=> V.emailFormat
          , passwordForLogin : V.required >=> V.minLength 2 >=> V.maxLength 20
          }
      
      F.handleSubmitValidate onSubmit F.validate validation

    render :: State -> H.ComponentHTML Action () m
    render { loginError, form: { formActions, fields, actions } } =
      HH.div_
        [ HH.h1
            [  ]
            [ HH.text "Sign In" ]
        , HH.p
            [  ]
            [ HH.a
                [ safeHref Register ] -- 
                [ HH.text "Need an account?" ]
            ]
        , HH.form
            [ HE.onSubmit formActions.handleSubmit ]
            [ whenElem loginError \_ ->
                HH.div
                  [  ]
                  [ HH.text "Email or password is invalid" ]
            , HH.fieldset_
                [ Field.textInput
                    { state: fields.emailForLogin, action: actions.emailForLogin }
                    [ HP.placeholder "Email"
                    , HP.type_ HP.InputEmail
                    ]
                , Field.textInput
                    { state: fields.passwordForLogin, action: actions.passwordForLogin }
                    [ HP.placeholder "Password"
                    , HP.type_ HP.InputPassword
                    ]
                , Field.submitButton "Log in"
                ]
            ]
        ]
