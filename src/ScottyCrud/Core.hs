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
import           Database.PostgreSQL.Simple
import           Control.Concurrent.Async

main' :: IO ()
main' = scotty 3000 $ do
  middleware $ checkRouteMiddleware 
  authController
  homeController

insertComment :: T.Text -> Int -> Int -> IO ()
insertComment commentContent postId userId = do
  conn <- getConn
  _ <- execute conn "insert into comments (comment_content,post_id,user_id) VALUES (?,?,?)" (commentContent,postId,userId) 
  close conn

fetchAllPosts :: IO [PostAndUserAndCat]
fetchAllPosts = do
  conn <- getConn
  postList <- query_ conn "select *from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id;" :: IO [PostAndUserAndCat]
  close conn
  pure postList

fetchPostById :: Int -> IO (Maybe PostAndUserAndCat)
fetchPostById postId = do
  conn <- getConn
  postList <- query conn "select *from posts join users on users.user_id = posts.user_id join category on posts.category_id = category.category_id where post_id = ?;" (Only postId) :: IO [PostAndUserAndCat]
  close conn
  case postList of
    [] -> pure Nothing
    [post] -> pure $ Just post

fetchCommentsByPostId :: Int -> IO [CommentAndUser]
fetchCommentsByPostId postId = do
  conn <- getConn
  commentList <- query conn "select *from comments join users on users.user_id = comments.user_id where post_id = ?;" (Only postId) :: IO [CommentAndUser]
  close conn
  pure commentList


searchText :: T.Text -> IO[PostAndUserAndCat]
searchText search_text = do
  conn <- getConn
  result <- query conn "SELECT * FROM posts JOIN users ON users.user_id = posts.user_id JOIN category ON posts.category_id = category.category_id WHERE post_title LIKE ? OR post_description LIKE ?;" (['%' <> search_term <> '%', '%' <> search_term <> '%']) :: IO [PostAndUserAndCat]
  close conn 
  return $ map (\(title, desciption) -> Posts title desription) result

homeController :: ScottyM ()
homeController = do
  middleware $ staticPolicy (addBase "/home/adming10x/Haskell-Training/Training/Project-04/Scotty-Crud")
  
  get ("/search") $ do 
    (search_term :: String) <- queryParam "search_term"
    case search_term of 
      Nothing -> redirect ("/")
      Just term -> do
        html $ renderHtml $ searchText search_text


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
        conn <- liftIO $ getConn
        _ <- liftIO $ execute conn "insert into posts (post_title,post_description,user_id,category_id) values (?,?,?,?);" (postTitle,postDescription,user_id user,categoryId)
        liftIO $ close conn
        redirect "/"
  get "/viewPost/:postId" $ do
    mUser <- getAuthUser
    postId' <- pathParam "postId"
    -- mPostInfo <- liftIO $ fetchPostById postId'
    -- commentList <- liftIO $ fetchCommentsByPostId postId'
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
        
{-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ScottyCrud.Core where
import           Web.Scotty
import           Database.PostgreSQL.Simple
import           Network.HTTP.Types
import           ScottyCrud.HTML
import           Text.Blaze.Renderer.Text
import           ScottyCrud.Common.Types
import           Web.Scotty.Cookie (getCookie, setSimpleCookie, makeSimpleCookie)
import qualified Data.Text.Read as TL (decimal)
import qualified Data.Text as T
import           Web.Cookie
import qualified Data.ByteString.Char8 as BS
import           Network.Wai.Middleware.Static

insertIntoTable :: Person' -> IO ()
insertIntoTable person = do
  conn <- getConn
  n <- execute conn "insert into persons (name,age) values (?,?);" person
  close conn

main' :: IO ()
main' = scotty 3000 $ do
  middleware $ staticPolicy (addBase "/home/user/haskell/Scotty-Crud/")
  get "/" $ do
    mCnt <- getCookie "count"
    cnt <- case mCnt of
      Nothing -> pure (0 :: Int)
      Just n -> case TL.decimal n of
        Right n' -> pure $ fst n'
        Left _   -> pure 0
    liftIO $ print ("webiste clicks:",cnt)
    let strCount = show (cnt+1)
    -- let cookie = defaultSetCookie { setCookieName = "count", setCookieValue = BS.pack strCount }
    -- setCookie cookie
    setSimpleCookie "count" (T.pack strCount)
    let res = renderHtml $ homePage Nothing
    html res
  -- get "/person/:id" -> Person 
  get "/persons" $ do
    conn <- liftIO $ getConn
    res <- liftIO $ (query_ conn "Select *FROM Persons;" :: IO [Person])
    liftIO $ close conn
    -- json $ re
    let res2 = (renderHtml $ personsPage res)
    html $ res2
  get "/insertPerson" $ do
    html $ renderHtml $ insertPersonPage
  post "/addPerson" $ do
    name <- formParam "name"
    age <- formParam "age"
    -- person <- jsonData
    let person = Person' name age
    liftIO $ insertIntoTable person
    text $ "Insertion successful"
  put "/updatePerson" $ do
    (person :: Person) <- jsonData
    liftIO $ print person
    conn <- liftIO getConn
    n <- liftIO $ execute conn "update persons set name = ?, age = ? where person_id = ?;" (name person,age person,getPersonId person)
    liftIO $ close conn
    if (n == 0) then text $ "updation failed" else text "updation done!"
    text $ "this is update request"
    {-
    res <- liftIO $ doesUserExist id
    case res of
      False -> text $ "User not found"
      _  -> do
                text $ "updation successful!! :)"
        -}
  {-
    /deletePerson/:id
  -}
  delete "/deletePerson/:tableName/:personId" $ do
    (res :: Int) <- pathParam "personId"
    (tableName :: String) <- pathParam "tableName"
    conn <- liftIO getConn
    n <- liftIO $ execute conn "delete from persons where person_id = ?" (Only res)
    liftIO $ close conn
    if (n == 0) then text "no user got delete" else text "all good"

  addroute GET "/good" $ do
    (res :: String) <- queryParam "name"
    (ageRes :: Maybe Int) <- queryParamMaybe "age"
    resList <- queryParams
    liftIO $ print resList
    resFile <- files
    liftIO $ print resFile
    case ageRes of
      Nothing -> liftIO $ print "sad"
      Just _  -> liftIO $ print "happy"
    liftIO $ print ("received as a query Param with Key as name",res)
    redirect "/notGood"

  matchAny "/notGood" $ do
    res <- body
    status status401
    liftIO $ print res
    html "<table></table><h1>Not good</h1>"
  notFound $ do
    text "the page you are looking for does not exist!"

getPersonId :: Person -> Int
getPersonId Person{..} = id

doesUserExist :: Int -> IO Bool
doesUserExist id = do
  conn <- getConn
  res <- query conn "select *from persons where person_id = ?" (Only id) :: IO [Person]
  close conn
  case res of
    (x:_) -> pure True
    _     -> pure False

-}
