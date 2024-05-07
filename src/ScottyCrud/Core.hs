{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ScottyCrud.Core where

import           Web.Scotty.Trans
import           ScottyCrud.Auth.Core
import           ScottyCrud.Middleware
import           Network.Wai.Middleware.Static
import           ScottyCrud.Handler
import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.Text.IO as TI
import qualified Data.ByteString.Lazy as BS
import           ScottyCrud.Common.Types
import qualified System.Environment as Sys
import           Control.Monad.Reader

main' :: IO ()
main' = do
  eAppSettings <- readAppSetting
  case eAppSettings of
    Left  e           -> TI.putStrLn e
    Right appSetting -> scottyT (port appSetting) (runIO appSetting) (application appSetting)
  where
    runIO :: AppSetting -> AppM a -> IO a
    runIO appSetting m = runReaderT (runAppM m) appSetting

    readAppSetting :: IO (Either Text AppSetting)
    readAppSetting = do
      args <- Sys.getArgs
      case args of
        []          -> pure $ Left "Please provide appsetting.json via command line"
        (appFile:_) -> do
          appFileContent <- BS.readFile appFile
          let mRes = A.decode appFileContent :: Maybe AppSetting
          case mRes of
            Nothing -> pure $ Left "Appsetting.json parsing failed, make sure all values are correct or not"
            Just res -> pure $ Right res

application :: AppSetting -> ScottyT AppM ()
application AppSetting{..} = do
  middleware checkRouteMiddleware
  middleware $ staticPolicy (addBase staticPath)
  authController
  homeController

homeController :: ScottyT AppM ()
homeController = do
  get    "/"                     getHomeR
  get    "/home/:pageNum"        getHomeR
  get    "/addPost"              getAddPostR
  get    "/admin"                getAdminR
  post   "/addPost"              postAddPostR
  get    "/viewPost/:postId"     getViewPostR
  post   "/addComment"           postAddCommentR
  post   "/deleteComment"        postDeleteCommentR
  get    "/updateComment"        getUpdateCommentR
  post   "/updateComment"        postUpdateCommentR
  get    "/deletePost/:postId"   getDeletePostR
  get    "/updatePost/:postId"   getUpdatePostR
  post   "/updatePost"           postUpdatePostR
  get    "/search"               getSearchR
  post   "/download"             postDownloadR