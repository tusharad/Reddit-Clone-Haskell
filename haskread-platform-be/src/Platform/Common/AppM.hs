{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Platform.Common.AppM
  ( AppM (..)
  , MyExceptT (..)
  , MonadHaxl (..)
  ) where

import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.Reader
import Haxl.Core (GenHaxl, runHaxl)
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.UnliftIO as OrvilleUnliftIO
import Platform.Common.Types
import Servant
import UnliftIO

-- Newtype wrapper around ExceptT
newtype MyExceptT e m a = MyExceptT
  { runMyExceptT :: ExceptT e m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError e
    )

instance Monad m => O.HasOrvilleState (AppM m) where
  askOrvilleState = AppM (asks appOrvilleState)
  localOrvilleState f (AppM reader_) =
    AppM $
      local
        ( \myAppState ->
            myAppState
              { appOrvilleState = f (appOrvilleState myAppState)
              }
        )
        reader_

instance (MonadIO m, UnliftIO.MonadUnliftIO m) => UnliftIO.MonadUnliftIO (AppM m) where
  withRunInIO inner = AppM $ ReaderT $ \r ->
    withRunInIO $ \runInIO ->
      inner (runInIO . runAppT r)

newtype AppM m a = AppM
  { getApp :: ReaderT MyAppState (MyExceptT ServerError m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ServerError
    , MonadReader MyAppState
    )

runAppT :: MyAppState -> AppM m a -> MyExceptT ServerError m a
runAppT r (AppM m) = runReaderT m r

instance forall m e. (MonadUnliftIO m, Exception e) => MonadUnliftIO (MyExceptT e m) where
  withRunInIO exceptToIO = MyExceptT $ ExceptT $ try $ do
    withRunInIO $ \runInIO ->
      exceptToIO (runInIO . (either throwIO pure <=< (runExceptT . runMyExceptT)))

-- https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL-UnliftIO.html#v:liftWithConnectionViaUnliftIO
instance (MonadIO m, UnliftIO.MonadUnliftIO m) => O.MonadOrvilleControl (AppM m) where
  liftWithConnection = OrvilleUnliftIO.liftWithConnectionViaUnliftIO
  liftCatch = OrvilleUnliftIO.liftCatchViaUnliftIO
  liftMask = OrvilleUnliftIO.liftMaskViaUnliftIO

instance (Monad m, MonadIO m, UnliftIO.MonadUnliftIO m) => O.MonadOrville (AppM m)

class Monad m => MonadHaxl m where
  runHaxlInM :: GenHaxl () () a -> m a

-- Implement MonadHaxl for your AppM
instance MonadIO m => MonadHaxl (AppM m) where
  -- runHaxlInM :: GenHaxl () () a -> AppM m a
  runHaxlInM haxlComp = do
    haxlEnv <- asks appHaxlEnv
    liftIO $ runHaxl haxlEnv haxlComp
