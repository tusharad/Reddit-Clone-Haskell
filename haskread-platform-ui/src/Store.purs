module Store where

import Prelude
import Data.Maybe (Maybe(..))

data LogLevel = LogDebug | LogInfo | LogError | LogWarn

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

newtype BaseURL = BaseURL String
data Profile = Profile {
    userID :: Int,
    userName :: String
 }

type Store = {
    logLevel :: LogLevel
  , baseUrl :: BaseURL
  , currentUser :: Maybe Profile
}

data Action
  = LoginUser Profile
  | LogoutUser

reduce :: Store -> Action -> Store
reduce store = case _ of
    LoginUser profile -> store { currentUser = Just profile }
    LogoutUser -> store { currentUser = Nothing }
