{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.Core where

import           Web.Scotty
import           ScottyCrud.Auth
import           ScottyCrud.Middleware
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import           Network.Wai.Middleware.Static
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           ScottyCrud.HTML
import           ScottyCrud.Common.Types
import           Control.Concurrent.Async
import           ScottyCrud.Query

main' :: IO ()
main' = scotty 3000 $ do
  middleware $ checkRouteMiddleware 
  authController
  homeController


homeController :: ScottyM ()
homeController = do
  middleware $ staticPolicy (addBase "/home/user/haskell/training/Scotty-Crud/")
  get "/" $ do
    mUser <- getAuthUser
    postList <- liftIO fetchAllPosts
    html $ renderHtml $ homePage mUser postList
  get "/addPost" $ do
    mUser <- getAuthUser
    case mUser of
      Nothing   -> redirect "/"
      Just _ -> html $ renderHtml $ addPostPage mUser
  get "/admin" $ do
    mUser <- getAuthUser
    case mUser of
      Nothing   -> redirect "/"
      Just user -> text $ "welcome " <> (TL.pack (user_email user))
  post "/addPost" $ do
    mUser <- getAuthUser
    (categoryId :: Int)       <- formParam "category_id"
    (postTitle :: T.Text)       <- formParam "post_title"
    (postDescription :: T.Text) <- formParam "post_description"
    case mUser of
      Nothing   -> redirect "/"
      Just user -> do
        liftIO $ addPostQ postTitle postDescription (user_id user) categoryId
        redirect "/"
  get "/viewPost/:postId" $ do
    mUser <- getAuthUser
    postId' <- pathParam "postId"
    (mPostInfo,commentList ) <- liftIO $ concurrently (fetchPostById postId') (fetchCommentsByPostId postId')
    case mPostInfo of
      Nothing -> redirect "/"
      Just postInfo -> html $ renderHtml $ viewPost mUser postInfo commentList
  post "/addComment" $ do
    mUser <- getAuthUser
    case mUser of
      Nothing -> redirect "/"
      Just user -> do
        (comment_content :: T.Text) <- formParam "comment_content"
        (post_id :: Int) <- formParam "post_id"
        let userId = user_id user
        liftIO $ insertComment comment_content post_id userId
        redirect $ "/viewPost/" <> TL.pack (show post_id)
