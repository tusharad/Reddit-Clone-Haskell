module Page.Register where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Effect.Aff.Class (class MonadAff)
import Capability.Resource (class ManageUser, registerUser,class Navigate, navigate)
import Common.Types (MyRoute(..))
import Common.Utils (safeHref,whenElem)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event
import Web.Event.Event (Event)
import Data.Either (Either(..),isLeft)
import Effect.Class.Console (log)

type Input = { redirect :: Boolean }

data Action
  = 
    SetUserName String
  | SetEmail String 
  | SetPassword String
  | SetConfirmPassword String
  | HandleSubmit Event

type State = {
      userName :: String
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

component :: 
  forall query output m. 
  MonadAff m =>
  Navigate m =>
  ManageUser m =>
  H.Component query Input output m
component = H.mkComponent {
        initialState,
        render,
        eval : H.mkEval H.defaultEval {
         handleAction = handleAction
        }
    }
  where
    initialState _ = {
        userName : "",
        email : "",
        password : "",
        confirmPassword : "",
        registerError : Right unit
    }

    handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
    handleAction = case _ of
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
                       let registerFields = {
                            userNameForRegister : st.userName
                           , emailForRegister : st.email
                           , passwordForRegister : st.password
                           , confirmPasswordForRegister : st.confirmPassword
                           }
                       eRes <- registerUser registerFields
                       case eRes of
                           Left _ -> log "Registration failed" *> pure unit 
                           Right userID -> navigate (OTP userID)

    render :: State -> H.ComponentHTML Action () m
    render st =
      HH.div_
        [ HH.h1
            [  ]
            [ HH.text "Sign Up" ]
        , HH.p
            [  ]
            [ HH.a
                [ safeHref Login] -- 
                [ HH.text "already have an account?" ]
            ]
        , HH.form
            [ HE.onSubmit HandleSubmit ]
            [ whenElem (isLeft st.registerError) \_ ->
                HH.div
                  [  ]
                  [ HH.text $ show st.registerError]
            , HH.fieldset_
                [ 
                  HH.input
                    [ 
                      HP.placeholder "User Name"
                    , HP.type_ HP.InputText
                    , HE.onValueInput SetUserName
                    , HP.value st.userName
                    ]
                , HH.input [ 
                      HP.placeholder "Email"
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
                , HH.input
                    [ HP.placeholder "Confirm Password"
                    , HP.type_ HP.InputPassword
                    , HE.onValueInput SetConfirmPassword
                    , HP.value st.confirmPassword
                    ]
                , HH.button 
                        [HP.type_ HP.ButtonSubmit]
                        [HH.text "Send OTP"]
                ]
            ]
        ]
