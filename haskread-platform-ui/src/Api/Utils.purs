module Api.Utils where

import Prelude

import Affjax.Web (request)
import Api.Request (RequestOptions, defaultRequest)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Profile (UserName)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen.Store.Monad (class MonadStore, getStore)
import Store (Action, Store)

mkRequest :: 
    forall m. MonadAff m => 
    MonadStore Action Store m => 
    RequestOptions -> 
    m (Maybe Json)
mkRequest opts = do
   {baseUrl} <- getStore
   response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
   pure $ hush $ rmap _.body response

-- TODO: Add logs upon receving errors
decode :: forall a. JsonCodec a -> Maybe Json -> Maybe a
decode _ Nothing = Nothing
decode codec (Just json) = case CA.decode codec json of
    Left _ -> Nothing
    Right response -> (Just response)

decodeWithUser 
    :: forall m a.
      MonadEffect m
   => MonadStore Action Store m
   => (Maybe UserName -> JsonCodec a)
   -> Maybe Json
   -> m (Maybe a)
decodeWithUser codec json = do
    {currentUser} <- getStore
    pure $ decode (codec (_.userName <$> currentUser)) json