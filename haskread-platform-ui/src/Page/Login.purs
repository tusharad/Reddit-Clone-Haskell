module Page.Login where

import Prelude
import Undefined (undefined)
import Halogen as H
import Halogen.HTML as HH

component :: forall query input output m. H.Component query input output m
component = H.mkComponent {
        initialState : identity,
        render,
        eval : H.mkEval H.defaultEval
    }
  where
    render :: forall state action slots. state -> H.ComponentHTML action slots m 
    render _ = HH.div_ [ HH.text "Login Page" ]
