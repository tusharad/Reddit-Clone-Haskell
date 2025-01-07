{- Module defining Server for manager -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Manager.Server
  ( startApp
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either
import qualified Data.Text.Lazy as T
import Manager.Common.Types
import Manager.Storage
import Manager.Systemd (restartService, stopService)
import Network.HTTP.Types (status200, status401, status500)
import System.Directory
import System.FilePath
import Web.Scotty

startApp :: ManagerConfig -> IO ()
startApp ManagerConfig {..} = scotty 5000 $
  post "/api/service/app/update" $ do
    authHeader <- header "Authorization"
    let tokenValid = case authHeader of
          Just t -> T.stripPrefix "Bearer " t == Just (T.pack bearerToken)
          Nothing -> False
    unless tokenValid $ do
      status status401
      text "Unauthorized"
      finish

    let downloadAndHandleError objectPath exeName = do
          liftIO $ putStrLn $ "Downloading " ++ exeName
          eResult <- liftIO $ downloadObject bucketName objectPath (downloadsPath </> exeName)
          case eResult of
            Left err -> status status500 >> text (T.pack err)
            Right _ -> return ()

    downloadAndHandleError uiObjectPath "ui-exe"
    downloadAndHandleError beObjectPath "be-exe"

    liftIO $ putStrLn "Stopping services"
    eServiceResults <- liftIO $ mapM stopService [uiServiceName, beServiceName]
    when (any isLeft eServiceResults) $ do
      status status500
      text $ T.pack $ "Service stop failed: " ++ concatMap (fromLeft "") eServiceResults

    liftIO $ do
      putStrLn "Copying executables"
      mapM_
        (uncurry copyFile)
        [(downloadsPath </> "ui-exe", uiLocation), (downloadsPath </> "be-exe", beLocation)]

    liftIO $ putStrLn "Restarting services"
    eRestartResults <- liftIO $ mapM restartService [uiServiceName, beServiceName]
    if any isLeft eRestartResults
      then
        status status500
          >> text
            ( T.pack $
                "Restart failed: "
                  ++ concatMap (fromLeft "") eRestartResults
            )
      else status status200 >> text "All good"
