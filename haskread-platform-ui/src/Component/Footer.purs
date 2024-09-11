module Component.Footer where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)
import Utils.Bulma (class_, classes_)
import Bulma.Layout.Layout as B
import Bulma.Elements.Elements as B
import Bulma.Modifiers.Typography as B

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
        HH.footer [class_ B.footer] [
            HH.div [classes_ [B.content,B.hasTextCentered]] [
              HH.p_ [
                HH.strong_ [ HH.text "HaskRead "]]
              , HH.text "by "
              , HH.a [HP.href "https://github.com/tusharad"] [HH.text "Tushar"]
            ],HH.p_ [HH.text "The project is Licensed under ",HH.a [HP.href "https://opensource.org/license/mit"] [HH.text "MIT"]]]

