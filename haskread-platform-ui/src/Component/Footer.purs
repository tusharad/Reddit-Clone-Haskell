module Component.Footer where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)

type State = { someVal :: Int }

component :: forall query output m.
  MonadAff m => H.Component query Unit output m
component = H.mkComponent {
    initialState 
    , render
    , eval : H.mkEval H.defaultEval
  }
  where
    initialState _ = { someVal : 0 }

    render :: forall state action. state -> H.ComponentHTML action () m
    render _ = 
        HH.div_ [
            HH.a 
             [ HP.href "https://github.com/tusharad/Reddit-Clone-Haskell"
             , HP.target "_blank"
             ] 
             [ HH.text "Checkout the source code!" ]
        ]

