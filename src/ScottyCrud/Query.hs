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

fetchSearchedPostsQ :: Text -> AppM [PostAndUserAndCat]
fetchSearchedPostsQ searchTerm = do
  dSetting <- asks dbSetting
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

getUserByIdQ :: Int -> AppM [User]
getUserByIdQ userId = do
  dSetting <- asks dbSetting
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
    
deletePostByIdQ :: Int -> AppM ()
deletePostByIdQ postId = do
  dSetting <- asks dbSetting
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql|
      DELETE 
      FROM 
        reddit_haskell.posts 
      WHERE 
        post_id = ?;
      |] (Only postId)
    pure ()

updatePostQ :: Text -> Text -> Int -> Int -> AppM ()
updatePostQ postTitle postDescription categoryId postId = do
  dSetting <- asks dbSetting
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql|
      UPDATE 
        reddit_haskell.posts 
      SET 
          post_title = ?
        , post_description = ?
        , category_id = ? 
      WHERE 
        post_id = ?;
      |] (postTitle,postDescription,categoryId,postId)
    pure ()

insertCommentQ :: T.Text -> Int -> Int -> Maybe Int -> AppM ()
insertCommentQ commentContent postId userId parentCommentId = do
  dSetting <- asks dbSetting
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

fetchAllPostsQ :: AppM [PostAndUserAndCat]
fetchAllPostsQ = do
  dSetting <- asks dbSetting
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

fetchPostByIdQ :: Int -> AppM (Maybe PostAndUserAndCat)
fetchPostByIdQ postId = do
  dSetting <- asks dbSetting
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

fetchCommentsByPostIdQ :: Int -> AppM [CommentAndUser]
fetchCommentsByPostIdQ postId = do
  dSetting <- asks dbSetting
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

fetchCommentByIdQ :: Int -> AppM [CommentAndUser]
fetchCommentByIdQ cId = do
  dSetting <- asks dbSetting
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

deleteCommentByIdQ :: Int -> AppM ()
deleteCommentByIdQ cId = do
  dSetting <- asks dbSetting
  liftIO $ withConnect (getConn dSetting) $ \conn -> do
    _ <- execute conn [sql| 
      DELETE 
      FROM 
        reddit_haskell.comments 
      WHERE 
        comment_id = ?;
      |] (Only cId)
    pure ()

addPostQ :: Text -> Text -> Int -> Int -> Maybe String -> AppM ()
addPostQ postTitle postDescription userId categoryId mFilePath = do
  dSetting <- asks dbSetting
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

fetchUserTokenQ :: Int -> AppM (Maybe Text)
fetchUserTokenQ uid = do
  dSetting <- asks dbSetting
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

verifyUserQ :: Int -> AppM ()
verifyUserQ userId = do
  dSetting <- asks dbSetting
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

fetchUserByUserNameQ :: Text -> AppM [User]
fetchUserByUserNameQ  userName = do
  dSetting <- asks dbSetting
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

fetchUserByEmailQ :: Text -> AppM [User]
fetchUserByEmailQ email = do
  dSetting <- asks dbSetting
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

fetchUserByIdQ :: Int -> AppM [User]
fetchUserByIdQ userId = do
  dSetting <- asks dbSetting
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

addUserQ :: Text -> Text -> Text -> Text -> AppM (Int)
addUserQ email password userName tokenVal = do
  dSetting       <- asks dbSetting
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

updateUserPasswordQ :: Int -> Text -> AppM ()
updateUserPasswordQ userId newPassword = do
  dSetting <- asks dbSetting
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
