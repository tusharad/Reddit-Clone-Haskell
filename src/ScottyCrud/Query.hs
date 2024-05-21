{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ScottyCrud.Query where

import qualified Data.Text as T
import           Data.Text (Text)
import           ScottyCrud.Common.Types
import           Database.PostgreSQL.Simple
import           Data.Password.Bcrypt
import           Control.Monad.Reader
import           Database.PostgreSQL.Simple.SqlQQ

fetchPostByPageQ :: DBSetting -> Int -> IO [PostAndUserAndCat]
fetchPostByPageQ dSetting n = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql|
      SELECT 
              posts.post_id
            , posts.post_title
            , posts.post_description
            , posts.user_id
            , posts.created_at
            , users.user_name
            , category.category_name
            , posts.file_path 
      FROM 
        reddit_haskell.posts JOIN reddit_haskell.users 
        ON 
          users.user_id = posts.user_id JOIN reddit_haskell.category
          ON posts.category_id = category.category_id
      LIMIT 10 OFFSET ? * 10;
        |] (Only (n-1)) :: IO [PostAndUserAndCat]

fetchSearchedPostsQ :: DBSetting -> Text -> IO [PostAndUserAndCat]
fetchSearchedPostsQ dSetting searchTerm = do
  liftIO $ liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql|
    SELECT 
        posts.post_id
      , posts.post_title
      , posts.post_description
      , posts.user_id
      , posts.created_at
      , users.user_name
      , category.category_name
      , posts.file_path 
    FROM 
      reddit_haskell.posts JOIN reddit_haskell.users 
      ON 
        users.user_id = posts.user_id JOIN reddit_haskell.category 
        ON 
        posts.category_id = category.category_id 
    WHERE 
      post_title LIKE ? OR post_description LIKE ?;
    |] ("%" <> searchTerm <> "%", "%" <> searchTerm <> "%")
      :: IO [PostAndUserAndCat]

getUserByIdQ :: DBSetting -> Int -> IO [User]
getUserByIdQ dSetting userId = do
  liftIO $ liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql|
      SELECT 
          user_id
        , user_email
        , password
        , user_name
        , is_verified
      FROM 
        reddit_haskell.users 
      WHERE 
        user_id = ?;
    |] (Only userId) :: IO [User]
    
deletePostByIdQ :: DBSetting -> Int -> IO ()
deletePostByIdQ dSetting postId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql|
      DELETE 
      FROM 
        reddit_haskell.posts 
      WHERE 
        post_id = ?;
      |] (Only postId)
    pure ()

updatePostQ :: DBSetting -> Text -> Text -> Int -> Int -> Maybe FilePath -> IO ()
updatePostQ dSetting postTitle postDescription categoryId postId mFilePath = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql|
      UPDATE 
        reddit_haskell.posts 
      SET 
          post_title       = ?
        , post_description = ?
        , category_id      = ? 
        , file_path         = ?
      WHERE 
        post_id = ?;
      |] (postTitle,postDescription,categoryId,mFilePath,postId)
    pure ()

insertCommentQ :: DBSetting -> T.Text -> Int -> Int -> Maybe Int -> IO ()
insertCommentQ dSetting commentContent postId userId parentCommentId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql|
      INSERT INTO 
        reddit_haskell.comments 
      (  
        comment_content
       , post_id,user_id
       , parent_comment_id
       ) 
        VALUES 
        (?,?,?,?)
    |] (commentContent,postId,userId,parentCommentId)
    pure ()

fetchAllPostsQ :: DBSetting -> IO [PostAndUserAndCat]
fetchAllPostsQ dSetting = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query_ conn [sql|
      SELECT 
              posts.post_id
            , posts.post_title
            , posts.post_description
            , posts.user_id
            , posts.created_at
            , users.user_name
            , category.category_name
            , posts.file_path 
      FROM 
        reddit_haskell.posts JOIN reddit_haskell.users 
        ON 
          users.user_id = posts.user_id JOIN reddit_haskell.category
          ON posts.category_id = category.category_id;
        |] :: IO [PostAndUserAndCat]

fetchPostByIdQ :: DBSetting -> Int -> IO (Maybe PostAndUserAndCat)
fetchPostByIdQ dSetting postId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    postList <- query conn [sql|
      SELECT 
          posts.post_id
        , posts.post_title
        , posts.post_description
        , posts.user_id
        , posts.created_at
        , users.user_name
        , category.category_name
        , posts.file_path 
      FROM 
        reddit_haskell.posts JOIN reddit_haskell.users 
        ON users.user_id = posts.user_id JOIN category 
          ON posts.category_id = category.category_id 
      WHERE 
        post_id = ?;
      |] (Only postId) :: IO [PostAndUserAndCat]
    case postList of
      [] -> pure Nothing
      [post] -> pure $ Just post

fetchCommentsByPostIdQ :: DBSetting -> Int -> IO [CommentAndUser]
fetchCommentsByPostIdQ dSetting postId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql|
      SELECT  comments.comment_id
            , comments.comment_content
            , comments.createdat
            , comments.user_id
            , users.user_name
            , comments.parent_comment_id
            , comments.post_id 
      FROM 
        reddit_haskell.comments JOIN reddit_haskell.users 
          ON users.user_id = comments.user_id 
      WHERE 
        post_id = ? 
      ORDER BY 
        comments.comment_id ASC;
      |] (Only postId) :: IO [CommentAndUser]

fetchCommentByIdQ :: DBSetting -> Int -> IO [CommentAndUser]
fetchCommentByIdQ dSetting cId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql|
      SELECT 
          comments.comment_id
        , comments.comment_content
        , comments.createdat
        , comments.user_id
        , users.user_name
        , comments.parent_comment_id
        , comments.post_id 
      FROM 
        reddit_haskell.comments JOIN reddit_haskell.users 
        ON users.user_id = comments.user_id 
      WHERE 
        comment_id = ? 
      ORDER BY 
        comments.comment_id ASC;
      |] (Only cId) :: IO [CommentAndUser]

deleteCommentByIdQ :: DBSetting -> Int -> IO ()
deleteCommentByIdQ dSetting cId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql| 
      DELETE 
      FROM 
        reddit_haskell.comments 
      WHERE 
        comment_id = ?;
      |] (Only cId)
    pure ()

addPostQ :: DBSetting -> Text -> Text -> Int -> Int -> Maybe String -> IO ()
addPostQ dSetting postTitle postDescription userId categoryId mFilePath = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
        _ <- execute conn [sql|
          INSERT INTO 
            reddit_haskell.posts 
          (
              post_title
            , post_description
            , user_id
            , category_id,file_path
          ) values (?,?,?,?,?); 
          |] (postTitle,postDescription,userId,categoryId,mFilePath)
        pure ()

fetchUserTokenQ :: DBSetting -> Int -> IO (Maybe Text)
fetchUserTokenQ dSetting uid = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    userList <- query conn [sql|
      SELECT 
        token 
      FROM 
        reddit_haskell.users 
      WHERE 
        user_id = ?;
    |] (Only uid) :: IO [(Only Text)]
    case userList of
      [] -> pure Nothing
      [(Only token)] -> pure $ Just token

verifyUserQ :: DBSetting -> Int -> IO ()
verifyUserQ dSetting userId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql|
      UPDATE 
        reddit_haskell.users 
      SET 
        is_verified = true 
      WHERE 
        user_id = ?;
      |] (Only userId)
    pure ()

fetchUserByUserNameQ :: DBSetting -> Text -> IO [User]
fetchUserByUserNameQ  dSetting userName = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql| 
      SELECT 
          user_id
        , user_email
        , password
        , user_name
        , is_verified 
      FROM 
        reddit_haskell.users 
      WHERE user_name = ?;
      |] (Only userName) :: IO [User]

fetchUserByEmailQ :: DBSetting -> Text -> IO [User]
fetchUserByEmailQ dSetting email = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql|
      SELECT user_id
            , user_email
            , password
            , user_name
            , is_verified 
      FROM 
        reddit_haskell.users 
      WHERE user_email = ?;
    |] (Only email) :: IO [User]

fetchUserByIdQ :: DBSetting -> Int -> IO [User]
fetchUserByIdQ dSetting userId = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query conn [sql|
      SELECT  user_id
            , user_email
            , password,user_name
            , is_verified 
      FROM 
        reddit_haskell.users 
      WHERE 
        user_id = ?;
      |] (Only userId):: IO [User]

addUserQ :: DBSetting -> Text -> Text -> Text -> Text -> IO (Int)
addUserQ dSetting email password userName tokenVal = do
  hashedPassword <- hashPassword $ mkPassword password
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    newUserId <- query conn [sql|
     INSERT INTO 
       reddit_haskell.users 
       ( user_email
        , password
        , user_name
        , token
        ) 
     VALUES (?,?,?,?) 
     RETURNING user_id;
     |] (email,hashedPassword,userName,tokenVal) :: IO [(Only Int)]
    let (Only uid) = head newUserId
    pure uid

updateUserPasswordQ :: DBSetting -> Int -> Text -> IO ()
updateUserPasswordQ dSetting userId newPassword = do
  hashedPassword <- hashPassword $ mkPassword newPassword
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql|
      UPDATE 
        reddit_haskell.users 
      SET 
        password = ? 
      WHERE 
        user_id = ?
    |] (hashedPassword,userId)
    pure ()

fetchAllCategoriesQ :: DBSetting -> IO ([Category])
fetchAllCategoriesQ dSetting = do
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    query_ conn [sql|
      SELECT 
        *
      FROM
        reddit_haskell.category;
    |] 
