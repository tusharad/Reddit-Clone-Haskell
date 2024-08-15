module Api.Request where

import Prelude

newtype Token = Token String

derive instance eqToken :: Eq Token
instance showToken :: Show Token where
    show (Token _) = "Token {- token -}"

newtype BaseURL = BaseURL String