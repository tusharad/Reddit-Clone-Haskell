module AppM
  ( AppM
  , runAppM
  )
  where

import Prelude

import Capability.Navigate (class Navigate)
import Capability.Resource (class ManageThreads,class ManageUser)
import Common.Types (Endpoint(..), RequestMethod(..), threadsCodec)
import Common.Types as Route
import Common.Utils (mkRequest, decode, authenticate,login,register,verifyOtp)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Store as Store

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

instance threadHalogenM :: ManageThreads AppM where
    getThreads = do
       mjson <- mkRequest { endpoint: Threads , method: Get }
       decode threadsCodec mjson

instance manageUserAppM :: ManageUser AppM where
  loginUser = authenticate login
  registerUser = register
  verifyOtp = verifyOtp
