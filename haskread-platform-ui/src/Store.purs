module Store where

import Prelude

import Data.Maybe (Maybe(..))
import Common.Types (BaseURL, Profile)

type Store =
  { baseUrl :: BaseURL
  , currentUser :: Maybe Profile
  }

data Action
  = LoginUser Profile
  | LogoutUser

reduce :: Store -> Action -> Store
reduce store = case _ of
  LoginUser profile -> store { currentUser = Just profile }
  LogoutUser -> store { currentUser = Nothing }