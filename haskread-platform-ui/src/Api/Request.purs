module Api.Request where

import Data.HTTP.Method
import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Affjax.Web (Request)
import Api.Endpoint (Endpoint, endpointCodec)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML.Properties (method)
import Routing.Duplex (print)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF

newtype Token = Token String

derive instance eqToken :: Eq Token
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype BaseURL = BaseURL String

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing