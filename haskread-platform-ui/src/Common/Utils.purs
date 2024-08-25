module Common.Utils where

import Prelude

import Affjax (printError,Error,Response)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (request, Request)
import Common.Types (BaseURL(..), Token(..), RequestOptions, RequestMethod(..), endpointCodec)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen.Store.Monad (class MonadStore, getStore)
import Routing.Duplex (print)
import Store (Action, Store)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Effect (Effect)
import Effect.Aff (Aff)
import Web.HTML (window)

mkRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case (rmap _.body response) of
    Left err -> (liftEffect $ log (printError err)) *> pure Nothing
    Right _ -> do
      pure $ hush $ rmap _.body response
 
mkAuthRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- getStore
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest baseUrl token opts
  pure $ hush $ map _.body response

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [RequestHeader "Authorization" $ "Bearer " <> t]
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

tokenKey = "token" :: String

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ Token <$> str

writeToken :: Token -> Effect Unit
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken =
  removeItem tokenKey =<< localStorage =<< window

decode :: forall m a. MonadEffect m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> (log $ "failed decodig: " <> (CA.printJsonDecodeError err)) *> pure Nothing
  Right response -> pure (Just response)
