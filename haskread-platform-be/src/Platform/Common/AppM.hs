{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Platform.Common.AppM (
   AppM(..),
   MyExceptT(..)
) where

import Platform.Common.Types
import Control.Monad.Reader
import Servant
import Control.Monad.Except
import qualified Orville.PostgreSQL.UnliftIO as OrvilleUnliftIO
import qualified Orville.PostgreSQL as O
import UnliftIO
import Control.Monad ((<=<))

-- Newtype wrapper around ExceptT
newtype MyExceptT e m a = MyExceptT {
  runMyExceptT :: ExceptT e m a
} deriving newtype (
        Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadError e
      )

instance Monad m => O.HasOrvilleState (AppM m) where
  askOrvilleState = AppM (asks appOrvilleState)
  localOrvilleState f (AppM reader_) = 
        AppM $ local (\myAppState -> myAppState {
            appOrvilleState = f (appOrvilleState myAppState)
        }) reader_

instance (MonadIO m,UnliftIO.MonadUnliftIO m) => UnliftIO.MonadUnliftIO (AppM m) where
  withRunInIO inner = AppM $ ReaderT $ \r ->
      withRunInIO $ \runInIO ->
        inner (runInIO . runAppT r)

newtype AppM m a = AppM { 
  getApp :: ReaderT MyAppState (MyExceptT ServerError m) a
} deriving newtype (
        Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadError ServerError
      )

runAppT :: MyAppState -> AppM m a -> MyExceptT ServerError m a
runAppT r (AppM m) = runReaderT m r

instance forall m e. (MonadUnliftIO m, Exception e) => MonadUnliftIO (MyExceptT e m) where
  withRunInIO exceptToIO = MyExceptT $ ExceptT $ try $ do
    withRunInIO $ \runInIO ->
      exceptToIO (runInIO . (either throwIO pure <=< (runExceptT . runMyExceptT)))

-- https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL-UnliftIO.html#v:liftWithConnectionViaUnliftIO
instance (MonadIO m,UnliftIO.MonadUnliftIO m) => O.MonadOrvilleControl (AppM m) where
  liftWithConnection = OrvilleUnliftIO.liftWithConnectionViaUnliftIO
  liftCatch = OrvilleUnliftIO.liftCatchViaUnliftIO
  liftMask = OrvilleUnliftIO.liftMaskViaUnliftIO

instance (Monad m,MonadIO m,UnliftIO.MonadUnliftIO m) => O.MonadOrville (AppM m) where

