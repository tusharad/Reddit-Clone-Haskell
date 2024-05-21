{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.Handler where

import           ScottyCrud.Auth.Handler
import           ScottyCrud.Query
import           Web.Scotty.Trans
import           Control.Monad.Reader
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           ScottyCrud.HTML.Core
import qualified Data.Text.Lazy as TL
import qualified ScottyCrud.Common.Types as PU (PostAndUserAndCat(..))
import qualified ScottyCrud.Common.Types as CU (CommentAndUser(..))
import           ScottyCrud.Common.Types hiding (MyData(..))
import qualified Data.Text as T
import           Network.Wai.Parse
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC
import           System.FilePath ((</>))

getHomeR :: (ActionT AppM ~ m) =>  m ()
getHomeR = do
    dSetting <- asks dbSetting
    (mPageNum :: Maybe Int) <- captureParamMaybe "pageNum"
    mMessage <- queryParamMaybe "message"
    case mPageNum of
      Nothing -> redirect "/home/1"
      Just pageNum -> do
        mUser <- getAuthUser
        postList <- liftIO $ fetchPostByPageQ dSetting pageNum
        categoryList <- liftIO $ fetchAllCategoriesQ dSetting
        html $ renderHtml $ homePage mUser postList categoryList pageNum mMessage

getAddPostR :: ActionT AppM ()
getAddPostR = do
    mUser <- getAuthUser
    dSetting <- asks dbSetting
    case mUser of
      Nothing   -> redirect "/"
      Just _ -> (liftIO $ fetchAllCategoriesQ dSetting) >>= (\categoryList -> html (renderHtml $ addPostPage mUser categoryList))

getAdminR :: ActionT AppM ()
getAdminR = do
    mUser <- getAuthUser
    case mUser of
      Nothing   -> redirect "/"
      Just user -> text $ "welcome " <> TL.pack (user_email user)

postAddPostR :: ActionT AppM ()
postAddPostR = do
    mUser <- getAuthUser
    (categoryId :: Int)         <- formParam "category_id"
    (postTitle :: T.Text)       <- formParam "post_title"
    (postDescription :: T.Text) <- formParam "post_description"
    fileList_                    <- files
    dSetting <- asks dbSetting
    let fileList = filter emptyFiles fileList_
    case mUser of
      Nothing   -> redirect "/"
      Just user -> do
        mFilePath <- checkAndWriteFile fileList
        _ <- liftIO $ addPostQ dSetting postTitle postDescription (user_id user) categoryId mFilePath
        redirect "/"
      where
        emptyFiles (_,fInfo) = (fileName fInfo) /= "\"\""

getViewPostR :: ActionT AppM ()
getViewPostR = do
    mUser <- getAuthUser
    postId' <- pathParam "postId"
    dSetting <- asks dbSetting
    mPostInfo   <- liftIO $ fetchPostByIdQ dSetting postId'
    commentList <- liftIO $ fetchCommentsByPostIdQ dSetting postId'
    case mPostInfo of
      Nothing -> redirect "/"
      Just postInfo -> html $ renderHtml $ viewPost mUser postInfo commentList

postAddCommentR :: ActionT AppM ()
postAddCommentR = do
    mUser <- getAuthUser
    case mUser of
      Nothing -> redirect "/"
      Just user -> do
        (comment_content :: T.Text)    <- formParam "comment_content"
        (post_id :: Int)               <- formParam "post_id"
        (parentCommentId :: Maybe Int) <- formParamMaybe "parent_comment_id"
        let userId = user_id user
        dSetting <- asks dbSetting
        liftIO $ insertCommentQ dSetting comment_content post_id userId parentCommentId
        redirect $ "/viewPost/" <> TL.pack (show post_id)

getDeletePostR :: ActionT AppM ()
getDeletePostR = do
    mUser  <- getAuthUser
    postId <- pathParam "postId"
    case mUser of
      Nothing   -> text "unauthorized!!"
      Just user -> do
          let userId' = user_id user
          dSetting <- asks dbSetting
          mPostInfo <- liftIO $ fetchPostByIdQ dSetting postId
          case mPostInfo of
            Nothing -> text "unauthorized!!"
            Just postInfo -> do
              (if PU.userId postInfo == userId' then liftIO (deletePostByIdQ dSetting postId) >> redirect "/" else text "unauthorized!")

getUpdatePostR :: ActionT AppM ()
getUpdatePostR = do
    mUser <- getAuthUser
    postId <- pathParam "postId"
    case mUser of
      Nothing   -> text "unauthorized!!"
      Just user -> do
          let userId' = user_id user
          dSetting <- asks dbSetting
          mPostInfo <- liftIO $ fetchPostByIdQ dSetting postId
          case mPostInfo of
            Nothing -> text "unauthorized!!"
            Just postInfo -> if
                              PU.userId postInfo == userId' then
                                html $ renderHtml $ updatePostPage mUser postInfo
                            else
                              text "unauthorized!"

postUpdatePostR :: ActionT AppM ()
postUpdatePostR = do
    mUser <- getAuthUser
    (categoryId :: Int)       <- formParam "category_id"
    (postId :: Int)       <- formParam "post_id"
    (postTitle :: T.Text)       <- formParam "post_title"
    (postDescription :: T.Text) <- formParam "post_description"
    fileList_                    <- files
    let fileList = filter emptyFiles fileList_
    case mUser of
      Nothing   -> text "unauthorized"
      Just user -> do
        dSetting <- asks dbSetting
        mPostInfo <- liftIO $ fetchPostByIdQ dSetting postId
        case mPostInfo of
            Nothing -> text "unauthorized!!"
            Just postInfo ->
              if
                PU.userId postInfo == user_id user then do
                  mFilePath <- checkAndWriteFile fileList
                  liftIO $ updatePostQ dSetting postTitle postDescription categoryId postId mFilePath
                  redirect "/?message=updation successful!"
              else
                text "unauthorized!"
    where
      emptyFiles (_,fInfo) = (fileName fInfo) /= "\"\""

checkAndWriteFile :: [(T.Text,FileInfo BS.ByteString)] -> ActionT AppM (Maybe FilePath)
checkAndWriteFile fileList = do
        uPath <- asks uploadPath
        case fileList of
            []      -> pure Nothing
            [(_,fInfo)] -> do
              let filePath = uPath </> BSC.unpack (fileName fInfo)
              liftIO $ BS.writeFile filePath (fileContent fInfo)
              pure (Just filePath)
            _       -> pure Nothing

getSearchR :: ActionT AppM ()
getSearchR = do
    search_term  <- queryParam "search_term"
    mUser        <- getAuthUser
    dSetting     <- asks dbSetting
    postList     <- liftIO $ fetchSearchedPostsQ dSetting search_term
    categoryList <- liftIO $ fetchAllCategoriesQ dSetting
    html $ renderHtml $ homePage mUser postList categoryList 1 Nothing

postDeleteCommentR :: ActionT AppM ()
postDeleteCommentR = do
  mUser <- getAuthUser
  commentId <- formParam "comment_id"
  case mUser of
    Nothing   -> redirect "/"
    Just user -> do
      dSetting <- asks dbSetting
      commentList <- liftIO $ fetchCommentByIdQ dSetting commentId
      case commentList of
        []  -> redirect "/"
        [c] -> do
          if user_id user /= CU.userId c then redirect "/" 
          else liftIO (deleteCommentByIdQ dSetting commentId)
          redirect "/"
        _   -> redirect "/" -- impossible case

getUpdateCommentR :: ActionT AppM ()
getUpdateCommentR = undefined

postUpdateCommentR :: ActionT AppM ()
postUpdateCommentR = undefined

postDownloadR :: ActionT AppM ()
postDownloadR = do
  filePath <- formParam "file_path"
  file filePath
