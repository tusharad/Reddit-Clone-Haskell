module AppM
  ( AppM(..)
  , runAppM
  )
  where

import Api.Endpoint
import Api.Request
import Api.Utils
import Prelude

import Api.Endpoint as E
import Api.Request (RequestOptions, defaultRequest)
import Api.Utils (decode, mkRequest)
import Capability.Navigate (class Navigate)
import Capability.Resource.Thread (class ManageThread)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)
import Data.Route as Route
import Data.Thread as TH
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Store (Action, Store)
import Store as Store
import Undefined (undefined)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: 
    forall query input output. Store.Store -> 
    H.Component query input output AppM -> 
    Aff (H.Component query input output Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  --TODO: implementing logout logic
  logout = undefined

instance manageThreadsAppM :: ManageThread AppM where
    getThreads =
        mkRequest { endpoint: E.Threads, method: Get }
            >>= (\mbJson -> pure $ decode TH.threadsCodec mbJson)