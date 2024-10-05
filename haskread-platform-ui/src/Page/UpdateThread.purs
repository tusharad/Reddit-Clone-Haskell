module Page.UpdateThread where

import Prelude

import Capability.Resource (class ManageThreads, getThread, updateThread, class Navigate, navigate)
import Common.Types (MyRoute(..), Thread, Profile)
import Common.Utils (defaultHomeOps, whenElem)
import Data.Either (Either(..), isLeft, fromLeft)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isNothing)
import Data.Maybe as Maybe
import Data.String (length)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Input = { threadID :: Int }

type State =
  { threadInfo :: RemoteData String Thread
  , threadID :: Int
  , threadTitle :: String
  , threadDescription :: String
  , threadCommunityID :: String
  , updateThreadError :: Either String Unit
  , currentUser :: Maybe Profile
  }

data Action
  = Initialize
  | LoadThread Int
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
  => MonadStore Store.Action Store.Store m
  => H.Component query Input output m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }
  where
  initialState { context: currentUser, input: { threadID } } =
    { threadID: threadID
    , threadTitle: ""
    , threadDescription: ""
    , threadCommunityID: "6" -- currently selected community ID
    , updateThreadError: Right unit
    , threadInfo: NotAsked
    , currentUser: currentUser
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      st <- H.get
      void $ H.fork $ handleAction (LoadThread st.threadID)

    LoadThread threadID -> do
      H.modify_ _ { threadInfo = Loading }
      mThreadList <- getThread threadID
      case mThreadList of
        Nothing -> pure unit
        Just threadInfo -> do
          -- check if user is owner of this thread or not.
          mCurrentUser <- H.gets _.currentUser
          case mCurrentUser of
            Nothing -> navigate (Home defaultHomeOps)
            Just currUser -> do
              when (currUser.userID /= threadInfo.userIDForThreadInfo)
                (navigate (Home defaultHomeOps))
          H.modify_ _
            { threadTitle = threadInfo.title
            , threadDescription = Maybe.fromMaybe "" threadInfo.description
            , threadCommunityID = show threadInfo.communityIDForThreadInfo
            }
      H.modify_ _ { threadInfo = fromMaybe mThreadList }

    SetThreadTitle tTitle ->
      H.modify_ _ { threadTitle = tTitle }

    SetThreadDescription tDescription ->
      H.modify_ _ { threadDescription = tDescription }

    SetCommunityID communityID ->
      H.modify_ _ { threadCommunityID = communityID }

    MakeRequest event -> do
      H.liftEffect $ Event.preventDefault event
      st <- H.get
      mRes <- case validateInput st of
        Left err -> do
          H.modify_ _ { updateThreadError = Left err }
          log err *> pure Nothing
        Right _ -> do
          let
            updateThreadFields =
              { threadIDForUpdate: st.threadID
              , threadTitleForUpdate: st.threadTitle
              , threadDescriptionForUpdate: st.threadDescription
              , threadCommunityIDForUpdate:
                  Maybe.fromMaybe (-1) (fromString st.threadCommunityID)
              }
          updateThread updateThreadFields
      case mRes of
        Nothing -> pure unit
        Just _ -> navigate (Home defaultHomeOps)

  render :: State -> H.ComponentHTML Action _ m
  render st =
    HH.div_
      [ HH.form
          [ HE.onSubmit MakeRequest ]
          [ whenElem (isLeft st.updateThreadError) \_ ->
              HH.div
                []
                [ HH.text $ fromLeft "" st.updateThreadError ]

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
