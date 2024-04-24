{-# LANGUAGE OverloadedStrings #-}

module ScottyCrud.Query where

import qualified Data.Text as T
import           Data.Text (Text) 
import           ScottyCrud.Common.Types
import           Database.PostgreSQL.Simple
import           Data.Password.Bcrypt
import           Control.Monad.Reader

fetchSearchedPostsQ :: Text -> AppM [PostAndUserAndCat]
fetchSearchedPostsQ searchTerm = do
  dSetting <- asks dbSetting
  liftIO $ liftIO $ withConnect (getConn dSetting) $ \conn -> do
    postList <- query conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_name,category.category_name,posts.file_path from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id WHERE post_title LIKE ? OR post_description LIKE ?;" ("%" <> (searchTerm ) <> "%", "%" <> (searchTerm ) <> "%") :: IO [PostAndUserAndCat]
    pure postList

getUserByIdQ :: Int -> AppM [User]
getUserByIdQ userId = do
  dSetting <- asks dbSetting 
  liftIO $ liftIO $ withConnect (getConn dSetting) $ \conn -> do
    userList <- (query conn "Select  user_id,user_email,password,user_name,is_verified FROM users where user_id = ?;" (Only userId):: IO [User])
    pure userList

deletePostByIdQ :: Int -> AppM ()
deletePostByIdQ postId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn "delete FROM posts where post_id = ?;" (Only postId)
    pure ()

updatePostQ :: Text -> Text -> Int -> Int -> AppM ()
updatePostQ postTitle postDescription categoryId postId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn "update posts set post_title = ?, post_description = ?, category_id = ? where post_id = ?;" (postTitle,postDescription,categoryId,postId)
    pure ()

insertCommentQ :: T.Text -> Int -> Int -> Maybe Int -> AppM ()
insertCommentQ commentContent postId userId parentCommentId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn "insert into comments (comment_content,post_id,user_id,parent_comment_id) VALUES (?,?,?,?)" (commentContent,postId,userId,parentCommentId)
    pure ()

fetchAllPostsQ :: AppM [PostAndUserAndCat]
fetchAllPostsQ = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    postList <- query_ conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_name,category.category_name,posts.file_path from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id;" :: IO [PostAndUserAndCat]
    pure postList

fetchPostByIdQ :: Int -> AppM (Maybe PostAndUserAndCat)
fetchPostByIdQ postId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    postList <- query conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_name,category.category_name,posts.file_path from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id where post_id = ?;" (Only postId) :: IO [PostAndUserAndCat]
    case postList of
      [] -> pure Nothing
      [post] -> pure $ Just post

fetchCommentsByPostIdQ :: Int -> AppM [CommentAndUser]
fetchCommentsByPostIdQ postId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    commentList <- query conn "select comments.comment_id,comments.comment_content,comments.createdat,comments.user_id,users.user_name,comments.parent_comment_id,comments.post_id from comments join users on users.user_id = comments.user_id where post_id = ? order by comments.comment_id asc;" (Only postId) :: IO [CommentAndUser]
    pure commentList

fetchCommentByIdQ :: Int -> AppM [CommentAndUser]
fetchCommentByIdQ cId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    commentList <- query conn "select comments.comment_id,comments.comment_content,comments.createdat,comments.user_id,users.user_name,comments.parent_comment_id,comments.post_id from comments join users on users.user_id = comments.user_id where comment_id = ? order by comments.comment_id asc;" (Only cId) :: IO [CommentAndUser]
    pure commentList

deleteCommentByIdQ :: Int -> AppM ()
deleteCommentByIdQ cId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn "delete from comments where comment_id = ?;" (Only cId)
    pure ()

addPostQ :: Text -> Text -> Int -> Int -> Maybe String -> AppM ()
addPostQ postTitle postDescription userId categoryId mFilePath = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
        _ <- execute conn "insert into posts (post_title,post_description,user_id,category_id,file_path) values (?,?,?,?,?);" (postTitle,postDescription,userId,categoryId,mFilePath)
        pure ()

fetchUserTokenQ :: Int -> AppM (Maybe Text)
fetchUserTokenQ uid = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    userList <- query conn "SELECT token FROM users where user_id = ?;" (Only uid) :: IO [(Only Text)]
    case userList of
      [] -> pure Nothing
      [(Only token)] -> pure $ Just token

verifyUserQ :: Int -> AppM ()
verifyUserQ userId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn "update users set is_verified = true where user_id = ?;" (Only userId)
    pure ()




fetchUserByUserNameQ :: Text -> AppM [User]
fetchUserByUserNameQ  userName = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    userList <- query conn "SELECT user_id,user_email,password,user_name,is_verified FROM users where user_name = ?;" (Only userName) :: IO [User]
    pure userList

fetchUserByEmailQ :: Text -> AppM [User]
fetchUserByEmailQ email = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    userList <- query conn "Select user_id,user_email,password,user_name,is_verified FROM users where user_email = ?;" (Only email):: IO [User]
    pure userList

fetchUserByIdQ :: Int -> AppM [User]
fetchUserByIdQ userId = do
  dSetting <- asks dbSetting 
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    userList <- query conn "Select user_id,user_email,password,user_name,is_verified FROM users where user_id = ?;" (Only userId):: IO [User]
    pure userList

addUserQ :: Text -> Text -> Text -> Text -> AppM (Int)
addUserQ email password userName tokenVal = do
  dSetting <- asks dbSetting 
  hashedPassword <- hashPassword $ mkPassword password
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
   newUserId <- query conn "insert into users (user_email,password,user_name,token) values (?,?,?,?) returning user_id;" (email,hashedPassword,userName,tokenVal) :: IO [(Only Int)]
   let (Only uid) = head newUserId
   pure $ uid

updateUserPasswordQ :: Int -> Text -> AppM ()
updateUserPasswordQ userId newPassword = do
  dSetting <- asks dbSetting 
  hashedPassword <- hashPassword $ mkPassword newPassword
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn "update users set password = ? where user_id = ?" (hashedPassword,userId)
    pure ()
