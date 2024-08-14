module Component.Router
  ( rootComponent
  )
  where

import Halogen as H
import Halogen.HTML as HH

type State = Int

data Action = Increment | Decrement

rootComponent :: forall query input output m. H.Component query input output m
rootComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.h1_ [HH.text "Hello World"] ]