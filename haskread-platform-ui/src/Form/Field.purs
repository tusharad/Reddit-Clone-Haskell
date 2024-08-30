module Form.Field where

import Prelude

import Form.Validation (FormError, errorToString)
import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

type StringField :: (Type -> Type -> Type -> Type) -> Type -> Type
type StringField f output = f String FormError output

submitButton :: forall i p. String -> HH.HTML i p
submitButton label =
  HH.input
    [
     HP.type_ HP.InputSubmit
    , HP.value label
    ]

type TextInput action output =
  { state :: F.FieldState String FormError output
  , action :: F.FieldAction action String FormError output
  }

textInput
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textInput { state, action } props =
  HH.fieldset
    [  ]
    [ HH.input
        ( append
            [ 
             HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    , maybeElem (state.result >>= either pure (const Nothing)) \err ->
        HH.div
          [  ]
          [ HH.text $ errorToString err ]
    ]

textarea
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLtextarea action)
  -> H.ComponentHTML action slots m
textarea { state, action } props =
  HH.fieldset
    [  ]
    [ HH.textarea
        ( append
            [ 
            HP.rows 8
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    ]
