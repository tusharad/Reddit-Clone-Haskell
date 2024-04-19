{-# LANGUAGE OverloadedStrings #-}

module ScottyCrud.Query where

import qualified Data.Text as T
import           Data.Text (Text) 
import           ScottyCrud.Common.Types
import           Database.PostgreSQL.Simple
import           Data.Password.Bcrypt

fetchSearchedPostsQ :: Text -> IO [PostAndUserAndCat]
fetchSearchedPostsQ searchTerm = do
  withConnect getConn $ \conn -> do
    postList <- query conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_name,category.category_name,posts.file_path from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id WHERE post_title LIKE ? OR post_description LIKE ?;" ("%" <> (searchTerm ) <> "%", "%" <> (searchTerm ) <> "%") :: IO [PostAndUserAndCat]
    pure postList

getUserByIdQ :: Int -> IO [User]
getUserByIdQ userId = do
  withConnect getConn $ \conn -> do
    userList <- (query conn "Select *FROM users where user_id = ?;" (Only userId):: IO [User])
    pure userList

deletePostByIdQ :: Int -> IO ()
deletePostByIdQ postId = do
  withConnect getConn $ \conn -> do
    _ <- execute conn "delete FROM posts where post_id = ?;" (Only postId)
    pure ()

updatePostQ :: Text -> Text -> Int -> Int -> Int -> IO ()
updatePostQ postTitle postDescription userId categoryId postId = do
  withConnect getConn $ \conn -> do
    _ <- execute conn "update posts set post_title = ?, post_description = ?, category_id = ? where post_id = ?;" (postTitle,postDescription,categoryId,postId)
    pure ()

insertCommentQ :: T.Text -> Int -> Int -> Maybe Int -> IO ()
insertCommentQ commentContent postId userId parentCommentId = do
  withConnect getConn $ \conn -> do
    _ <- execute conn "insert into comments (comment_content,post_id,user_id,parent_comment_id) VALUES (?,?,?,?)" (commentContent,postId,userId,parentCommentId)
    pure ()

fetchAllPostsQ :: IO [PostAndUserAndCat]
fetchAllPostsQ = do
  withConnect getConn $ \conn -> do
    postList <- query_ conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_name,category.category_name,posts.file_path from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id;" :: IO [PostAndUserAndCat]
    pure postList

fetchPostByIdQ :: Int -> IO (Maybe PostAndUserAndCat)
fetchPostByIdQ postId = do
  withConnect getConn $ \conn -> do
    postList <- query conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_name,category.category_name,posts.file_path from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id where post_id = ?;" (Only postId) :: IO [PostAndUserAndCat]
    case postList of
      [] -> pure Nothing
      [post] -> pure $ Just post

fetchCommentsByPostIdQ :: Int -> IO [CommentAndUser]
fetchCommentsByPostIdQ postId = do
  withConnect getConn $ \conn -> do
    commentList <- query conn "select comments.comment_id,comments.comment_content,comments.createdat,comments.user_id,users.user_name,comments.parent_comment_id,comments.post_id from comments join users on users.user_id = comments.user_id where post_id = ? order by comments.comment_id asc;" (Only postId) :: IO [CommentAndUser]
    pure commentList

fetchCommentByIdQ :: Int -> IO [CommentAndUser]
fetchCommentByIdQ cId = do
  withConnect getConn $ \conn -> do
    commentList <- query conn "select comments.comment_id,comments.comment_content,comments.createdat,comments.user_id,users.user_name,comments.parent_comment_id,comments.post_id from comments join users on users.user_id = comments.user_id where comment_id = ? order by comments.comment_id asc;" (Only cId) :: IO [CommentAndUser]
    pure commentList

deleteCommentByIdQ :: Int -> IO ()
deleteCommentByIdQ cId = do
  withConnect getConn $ \conn -> do
    _ <- execute conn "delete from comments where comment_id = ?;" (Only cId)
    pure ()

addPostQ :: Text -> Text -> Int -> Int -> Maybe String -> IO ()
addPostQ postTitle postDescription userId categoryId mFilePath = do
  withConnect getConn $ \conn -> do
        _ <- execute conn "insert into posts (post_title,post_description,user_id,category_id,file_path) values (?,?,?,?,?);" (postTitle,postDescription,userId,categoryId,mFilePath)
        pure ()

fetchUserByUserNameQ :: Text -> IO [User]
fetchUserByUserNameQ  userName = do
  withConnect getConn $ \conn -> do
    userList <- query conn "SELECT *FROM users where user_name = ?;" (Only userName) :: IO [User]
    pure userList

fetchUserByEmailQ :: Text -> IO [User]
fetchUserByEmailQ email = do
  withConnect getConn $ \conn -> do
    userList <- query conn "Select *FROM users where user_email = ?;" (Only email):: IO [User]
    pure userList

fetchUserByIdQ :: Int -> IO [User]
fetchUserByIdQ userId = do
  withConnect getConn $ \conn -> do
    userList <- query conn "Select *FROM users where user_id = ?;" (Only userId):: IO [User]
    pure userList

addUserQ :: Text -> Text -> Text -> IO ()
addUserQ email password userName = do
  hashedPassword <- hashPassword $ mkPassword password
  withConnect getConn $ \conn -> do
   _ <- execute conn "insert into users (user_email,password,user_name) values (?,?,?);" (email,hashedPassword,userName)
   pure ()

updateUserPasswordQ :: Int -> Text -> IO ()
updateUserPasswordQ userId newPassword = do
  hashedPassword <- hashPassword $ mkPassword newPassword
  withConnect getConn $ \conn -> do
    _ <- execute conn "update users set password = ? where user_id = ?" (hashedPassword,userId)
    pure ()
