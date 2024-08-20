module Common.Utils where

import Prelude
import Data.Maybe (Maybe(..)) 
import Data.Argonaut.Core (Json)
import Halogen.Store.Monad (class MonadStore, getStore)
import Effect.Aff.Class (class MonadAff, liftAff)
import Affjax.Web (request,Request)
import Data.Bifunctor (rmap)
import Data.Either (Either(..),hush)

import Common.Types (BaseURL(..)
    ,Token(..),RequestOptions,RequestMethod(..),endpointCodec)
import Data.Tuple (Tuple(..))
import Routing.Duplex (print)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Data.HTTP.Method (Method(..))
import Store (Action,Store)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Effect.Class (class MonadEffect)

mkRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ hush $ rmap _.body response

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

decode :: forall m a. MonadEffect m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left _ -> pure Nothing
  Right response -> pure (Just response)
