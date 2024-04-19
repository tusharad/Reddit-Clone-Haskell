{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.Handler where

import           ScottyCrud.Auth.Handler
import           ScottyCrud.Query
import           Web.Scotty
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           ScottyCrud.HTML.Core
import qualified Data.Text.Lazy as TL
import qualified ScottyCrud.Common.Types as PU (PostAndUserAndCat(..))
import qualified ScottyCrud.Common.Types as CU (CommentAndUser(..))
import           Control.Concurrent
import           Control.Concurrent.Async
import           ScottyCrud.Common.Types
import qualified Data.Text as T
import           Network.Wai.Parse
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC

getHomeR :: ActionM ()
getHomeR = do
    mUser <- getAuthUser
    postList <- liftIO $ fetchAllPostsQ
    html $ renderHtml $ homePage mUser postList

getAddPostR :: ActionM ()
getAddPostR = do
    mUser <- getAuthUser
    case mUser of
      Nothing   -> redirect "/"
      Just _ -> html $ renderHtml $ addPostPage mUser

getAdminR :: ActionM ()
getAdminR = do
    mUser <- getAuthUser
    case mUser of
      Nothing   -> redirect "/"
      Just user -> text $ "welcome " <> (TL.pack (user_email user))

postAddPostR :: ActionM ()
postAddPostR = do
    mUser <- getAuthUser
    (categoryId :: Int)       <- formParam "category_id"
    (postTitle :: T.Text)       <- formParam "post_title"
    (postDescription :: T.Text) <- formParam "post_description"
    fileList                            <- files
    liftIO $ print fileList
    
    case mUser of
      Nothing   -> redirect "/"
      Just user -> do
        mFilePath <- case fileList of
            [] -> pure Nothing
            [file_] -> do
              let fInfo = snd file_
              liftIO $ BS.writeFile ("/home/user/haskell/training/Scotty-Crud/uploads/" <> BSC.unpack (fileName fInfo)) (fileContent fInfo)
              pure $ Just $ "/home/user/haskell/training/Scotty-Crud/uploads/" <> BSC.unpack (fileName fInfo)
        _ <- liftIO $ forkIO $ addPostQ postTitle postDescription (user_id user) categoryId mFilePath
        redirect "/"

getViewPostR :: ActionM ()
getViewPostR = do
    mUser <- getAuthUser
    postId' <- pathParam "postId"
    (mPostInfo,commentList ) <- liftIO $ concurrently (fetchPostByIdQ postId') (fetchCommentsByPostIdQ postId')
    case mPostInfo of
      Nothing -> redirect "/"
      Just postInfo -> html $ renderHtml $ viewPost mUser postInfo commentList

postAddCommentR :: ActionM ()
postAddCommentR = do
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

getDeletePostR :: ActionM ()
getDeletePostR = do
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

getUpdatePostR :: ActionM ()
getUpdatePostR = do
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

postUpdatePostR :: ActionM ()
postUpdatePostR = do
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

getSearchR :: ActionM ()
getSearchR = do
    search_term <- queryParam "search_term"
    mUser       <- getAuthUser
    postList    <- liftIO $ fetchSearchedPostsQ search_term
    html $ renderHtml $ homePage mUser postList

postDeleteCommentR :: ActionM ()
postDeleteCommentR = do
  mUser <- getAuthUser
  commentId <- formParam "comment_id"
  case mUser of
    Nothing   -> redirect "/"
    Just user -> do
      commentList <- liftIO $ fetchCommentByIdQ commentId
      case commentList of
        [] -> redirect "/"
        [c] -> if (user_id user /= CU.userId c) then redirect "/" else (liftIO $ deleteCommentByIdQ commentId) >> redirect "/"

getUpdateCommentR :: ActionM ()
getUpdateCommentR = undefined

postUpdateCommentR :: ActionM ()
postUpdateCommentR = undefined

postDownloadR :: ActionM ()
postDownloadR = do
  filePath <- formParam "file_path"
  file filePath
