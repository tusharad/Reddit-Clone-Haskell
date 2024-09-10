module Component.ThreadView where

import Common.Types
import Prelude

import Bulma.Components.Card as B
import Bulma.Layout.Layout as B
import Bulma.Elements.Title as B
import Data.Maybe (fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Utils.Bulma (class_, classes_)
import Bulma.Columns.Size as B
import Bulma.CSS.Spacing as B
import Bulma.Modifiers.Typography as B

type State = { thread :: ThreadInfo }
type Input = { thread :: ThreadInfo }

component
  :: forall query output m
   . MonadAff m
  => H.Component query Input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
  initialState { thread: t } = { thread: t }

  render :: forall action. State -> H.ComponentHTML action () m
  render { thread: t } =
    HH.div
      [ class_ B.card ]
      [ HH.header
          [ class_ B.cardHeader ]
          [ HH.div
              [ class_ B.mediaContent ]
              [ HH.p
                  [ class_ B.cardHeaderTitle ]
                  [ HH.text t.title ]
              ]
          , HH.a
              [ classes_ [ B.subtitle, B.is6, B.px4, B.hasTextCentered ] ]
              [ HH.text t.userNameForThreadInfo
              , HH.p_ [HH.text $ fromMaybe "Something went wrong :(" t.age] ]
          ]
      ]

