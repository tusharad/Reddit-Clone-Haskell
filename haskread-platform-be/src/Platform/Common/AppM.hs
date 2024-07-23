{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Platform.Common.AppM (
   AppM(..)
) where

import Platform.Common.Types
import Control.Monad.Reader
import Servant
import Control.Monad.Except
import qualified Orville.PostgreSQL.UnliftIO as OrvilleUnliftIO
import qualified Orville.PostgreSQL as O
import UnliftIO
import Control.Monad ((<=<))

type ServerExcept m = ExceptT ServerError m

newtype AppM m a = AppM { 
  getApp :: ReaderT MyAppState (ServerExcept m) a
} deriving newtype (
        Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadError ServerError
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

runAppT :: MyAppState -> AppM m a -> ServerExcept m a
runAppT r (AppM m) = runReaderT m r

-- taken from https://github.com/freckle/yesod-auth-oauth2/blob/acb69f8da40b9c91b4020296ce105119e76fdf1d/src/UnliftIO/Except.hs#L9
instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
  withRunInIO exceptToIO = ExceptT $ try $ do
    withRunInIO $ \runInIO ->
      exceptToIO (runInIO . (either throwIO pure <=< runExceptT))

-- https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL-UnliftIO.html#v:liftWithConnectionViaUnliftIO
instance (MonadIO m,UnliftIO.MonadUnliftIO m) => O.MonadOrvilleControl (AppM m) where
  liftWithConnection = OrvilleUnliftIO.liftWithConnectionViaUnliftIO
  liftCatch = OrvilleUnliftIO.liftCatchViaUnliftIO
  liftMask = OrvilleUnliftIO.liftMaskViaUnliftIO

instance (Monad m,MonadIO m,UnliftIO.MonadUnliftIO m) => O.MonadOrville (AppM m) where

