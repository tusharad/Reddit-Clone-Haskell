{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.Core where

import           Web.Scotty
import           ScottyCrud.Auth
import           ScottyCrud.Middleware
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import           Network.Wai.Middleware.Static
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           ScottyCrud.HTML.Core
import           ScottyCrud.Common.Types
import qualified ScottyCrud.Common.Types as PU (PostAndUserAndCat(..))
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
    postList <- liftIO fetchAllPostsQ
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
    (mPostInfo,commentList ) <- liftIO $ concurrently (fetchPostByIdQ postId') (fetchCommentsByPostIdQ postId')
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
        (parentCommentId :: Maybe Int) <- formParamMaybe "parent_comment_id"
        let userId = user_id user
        liftIO $ insertCommentQ comment_content post_id userId parentCommentId
        redirect $ "/viewPost/" <> TL.pack (show post_id)
  get "/deletePost/:postId" $ do
    mUser <- getAuthUser
    postId <- pathParam "postId"
    case mUser of
      Nothing   -> text "unauthorized!!"
      Just user -> do
          let userId' = user_id user
          mPostInfo <- liftIO $ fetchPostByIdQ postId
          case mPostInfo of
            Nothing -> text "unauthorized!!"
            Just postInfo -> do
              case ((PU.userId postInfo) == userId') of
               False -> text "unauthorized!"
               True  -> (liftIO $ deletePostByIdQ postId) >> redirect "/"
  get "/updatePost/:postId" $ do
    mUser <- getAuthUser
    postId <- pathParam "postId"
    case mUser of
      Nothing   -> text "unauthorized!!"
      Just user -> do
          let userId' = user_id user
          mPostInfo <- liftIO $ fetchPostByIdQ postId
          case mPostInfo of
            Nothing -> text "unauthorized!!"
            Just postInfo -> do
              case ((PU.userId postInfo) == userId') of
               False -> text "unauthorized!"
               True  -> html $ renderHtml $ updatePostPage mUser postInfo
  post "/updatePost" $ do
    mUser <- getAuthUser
    (categoryId :: Int)       <- formParam "category_id"
    (postId :: Int)       <- formParam "post_id"
    (postTitle :: T.Text)       <- formParam "post_title"
    (postDescription :: T.Text) <- formParam "post_description"
    case mUser of
      Nothing   -> text $ "unauthorized"
      Just user -> do
        mPostInfo <- liftIO $ fetchPostByIdQ postId
        case mPostInfo of
            Nothing -> text "unauthorized!!"
            Just postInfo -> do
              case ((PU.userId postInfo) == (user_id user)) of
               False -> text "unauthorized!"
               True  -> do
                liftIO $ updatePostQ postTitle postDescription (user_id user) categoryId postId
                redirect "/"
  get "/search" $ do
    search_term <- queryParam "search_term"
    mUser <- getAuthUser
    postList <- liftIO $ fetchSearchedPostsQ search_term
    html $ renderHtml $ homePage mUser postList
      

