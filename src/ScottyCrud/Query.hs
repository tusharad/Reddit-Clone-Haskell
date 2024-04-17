{-# LANGUAGE OverloadedStrings #-}

module ScottyCrud.Query where

import qualified Data.Text as T
import           Data.Text (Text) 
import           ScottyCrud.Common.Types
import           Database.PostgreSQL.Simple


fetchSearchedPostsQ :: Text -> IO [PostAndUserAndCat]
fetchSearchedPostsQ searchTerm = do
  conn <- getConn
  postList <- query conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_email,category.category_name from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id WHERE post_title LIKE ? OR post_description LIKE ?;" ("%" <> (searchTerm ) <> "%", "%" <> (searchTerm ) <> "%") :: IO [PostAndUserAndCat]
  close conn
  pure postList

getUserByIdQ :: Int -> IO [User]
getUserByIdQ userId = do
    conn <- getConn
    userList <- (query conn "Select *FROM users where user_id = ?;" (Only userId):: IO [User])
    close conn
    pure userList

deletePostByIdQ :: Int -> IO ()
deletePostByIdQ postId = do
    conn <- getConn
    _ <- execute conn "delete FROM posts where post_id = ?;" (Only postId)
    close conn

updatePostQ :: Text -> Text -> Int -> Int -> Int -> IO ()
updatePostQ postTitle postDescription userId categoryId postId = do
    conn <- getConn
    _ <- execute conn "update posts set post_title = ?, post_description = ?, category_id = ? where post_id = ?;" (postTitle,postDescription,categoryId,postId)
    close conn

insertCommentQ :: T.Text -> Int -> Int -> Maybe Int -> IO ()
insertCommentQ commentContent postId userId parentCommentId = do
  conn <- getConn
  _ <- execute conn "insert into comments (comment_content,post_id,user_id,parent_comment_id) VALUES (?,?,?,?)" (commentContent,postId,userId,parentCommentId) 
  close conn

fetchAllPostsQ :: IO [PostAndUserAndCat]
fetchAllPostsQ = do
  conn <- getConn
  postList <- query_ conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_email,category.category_name from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id;" :: IO [PostAndUserAndCat]
  close conn
  pure postList

fetchPostByIdQ :: Int -> IO (Maybe PostAndUserAndCat)
fetchPostByIdQ postId = do
  conn <- getConn
  postList <- query conn "select posts.post_id,posts.post_title,posts.post_description,posts.user_id,posts.created_at,users.user_email,category.category_name from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id where post_id = ?;" (Only postId) :: IO [PostAndUserAndCat]
  close conn
  case postList of
    [] -> pure Nothing
    [post] -> pure $ Just post

fetchCommentsByPostIdQ :: Int -> IO [CommentAndUser]
fetchCommentsByPostIdQ postId = do
  conn <- getConn
  commentList <- query conn "select comments.comment_id,comments.comment_content,comments.createdat,comments.user_id,users.user_email,comments.parent_comment_id,comments.post_id from comments join users on users.user_id = comments.user_id where post_id = ? order by comments.comment_id asc;" (Only postId) :: IO [CommentAndUser]
  close conn
  pure commentList

addPostQ :: Text -> Text -> Int -> Int -> IO ()
addPostQ postTitle postDescription userId categoryId = do
        conn <- getConn
        _ <- execute conn "insert into posts (post_title,post_description,user_id,category_id) values (?,?,?,?);" (postTitle,postDescription,userId,categoryId)
        close conn

fetchUserByEmailQ :: Text -> IO [User]
fetchUserByEmailQ email = do
    conn <- getConn
    userList <- query conn "Select *FROM users where user_email = ?;" (Only email):: IO [User]
    close conn
    pure userList

addUserQ :: Text -> Text -> IO ()
addUserQ email password = do
  conn <- getConn
  _ <- execute conn "insert into users (user_email,password) values (?,?);" (email,password)
  close conn
