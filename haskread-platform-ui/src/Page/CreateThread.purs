module Page.CreateThread where

import Prelude

import Capability.Resource (class ManageThreads, createThread, class Navigate, navigate)
import Common.Types (MyRoute(..))
import Common.Utils (defaultHomeOps, whenElem)
import Data.Either (Either(..), isLeft, fromLeft)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.String (length)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State =
  { threadTitle :: String
  , threadDescription :: String
  , threadCommunityID :: String
  , createThreadError :: Either String Unit
  }

data Action
  = Initialize
  | HandleSubmit
  | SetThreadTitle String
  | SetThreadDescription String
  | SetCommunityID String
  | MakeRequest Event

validateInput :: State -> Either String Unit
validateInput { threadTitle, threadDescription, threadCommunityID } = do
  if (threadTitle == mempty) then Left "Title required"
  else if ((length threadDescription) > 256) then Left "Description too long"
  else if (isNothing $ fromString threadCommunityID) then Left "Community ID invalid"
  else (Right unit)

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageThreads m
  => H.Component query Unit output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }
  where
  initialState _ =
    { threadTitle: ""
    , threadDescription: ""
    , threadCommunityID: "6" -- currently selected community ID
    , createThreadError: Right unit
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> pure unit
    HandleSubmit -> pure unit
    SetThreadTitle tTitle ->
      H.modify_ (_ { threadTitle = tTitle })
    SetThreadDescription tDescription ->
      H.modify_ _ { threadDescription = tDescription }
    SetCommunityID communityID ->
      H.modify_ _ { threadCommunityID = communityID }
    MakeRequest event -> do
      H.liftEffect $ Event.preventDefault event
      st <- H.get
      mRes <- case validateInput st of
        Left err -> do
          H.modify_ _ { createThreadError = Left err }
          log err *> pure Nothing
        Right _ -> do
          let
            createThreadFields =
              { threadTitleForCreate: st.threadTitle
              , threadDescriptionForCreate: st.threadDescription
              , threadCommunityIDForCreate:
                  fromMaybe (-1) (fromString st.threadCommunityID)
              }
          createThread createThreadFields
      case mRes of
        Nothing -> pure unit
        Just _ -> navigate (Home defaultHomeOps )

  render :: State -> H.ComponentHTML Action _ m
  render st =
    HH.div_
      [ HH.form
          [ HE.onSubmit MakeRequest ]
          [ whenElem (isLeft st.createThreadError) \_ ->
              HH.div
                []
                [ HH.text $ fromLeft "" st.createThreadError ]

          , HH.label [] [ HH.text "Title" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput SetThreadTitle
              , HP.value st.threadTitle
              ]
          , HH.textarea
              [ HE.onValueInput SetThreadDescription
              , HP.value st.threadDescription
              ]
          , HH.select
              [ HE.onValueInput SetCommunityID ]
              [ HH.option
                  [ HP.value "6"
                  , HP.selected $ "6" == st.threadCommunityID
                  ]
                  [ HH.text "Haskell" ]
              , HH.option
                  [ HP.value "7"
                  , HP.selected $ "7" == st.threadCommunityID
                  ]
                  [ HH.text "Javascript" ]
              ]
          , HH.button
              [ HP.type_ HP.ButtonSubmit ]
              [ HH.text "submit" ]
          ]
      ]
