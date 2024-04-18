{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.Core where

import           Web.Scotty
import           ScottyCrud.Auth.Core
import           ScottyCrud.Middleware
import           Network.Wai.Middleware.Static
import           ScottyCrud.Handler

main' :: IO ()
main' = scotty 3000 $ do
  middleware $ checkRouteMiddleware 
  authController
  homeController

homeController :: ScottyM ()
homeController = do
  middleware $ staticPolicy (addBase "/home/user/haskell/training/Scotty-Crud/")
  get "/"                   getHomeR
  get "/addPost"            getAddPostR
  get "/admin"              getAdminR
  post "/addPost"           postAddPostR
  get "/viewPost/:postId"   getViewPostR
  post "/addComment"        postAddCommentR
  get "/deletePost/:postId" getDeletePostR
  get "/updatePost/:postId" getUpdatePostR
  post "/updatePost"        postUpdatePostR 
  get "/search"             getSearchR
