module AppM
  ( AppM(..)
  , runAppM
  )
  where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect,liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Safe.Coerce (coerce)
import Store as Store
import Capability.Navigate (class Navigate)
import Routing.Hash (setHash)
import Routing.Duplex (print)
import Common.Types as Route

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
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance navigateHalogenM :: Navigate AppM where
   navigate =
     liftEffect <<< setHash <<< print Route.myRoute
