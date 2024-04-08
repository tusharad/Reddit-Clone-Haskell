{-# LANGUAGE OverloadedStrings #-}

module ScottyCrud.Auth where
import           Web.Scotty
import           Web.Scotty.Cookie
import           ScottyCrud.HTML
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Database.PostgreSQL.Simple
import           ScottyCrud.Common.Types
import qualified Data.Text as T
import           Web.JWT
import qualified Data.Map.Strict as Map
import           Data.Aeson
import           Data.Scientific (toBoundedInteger)
import           Data.Maybe (fromMaybe)

{-
    scotty
  , yesod
  , servant
  , webapi
  , wai 
-}

getAuthUser :: ActionM (Maybe User)
getAuthUser = do
    mUserId <- getCookie "auth_token"
    mUser <- liftIO $ case mUserId of
        Nothing -> pure Nothing
        Just encryptedData -> do
           let mRes = fmap claims $ decodeAndVerifySignature (toVerify . hmacSecret . T.pack $ "hello cat") encryptedData
           case mRes of
              Nothing -> pure Nothing
              Just res -> case Map.lookup "user_id" $ unClaimsMap $ unregisteredClaims res of
                    Nothing -> pure Nothing
                    Just (Number user_id) -> do
                      conn <- liftIO getConn
                      let user_id' = fromMaybe (0 :: Int) $ toBoundedInteger user_id
                      userList <- liftIO (query conn "Select *FROM users where user_id = ?;" (Only user_id'):: IO [User])
                      close conn
                      case userList of
                        [] -> pure Nothing
                        [user] -> pure $ Just user
    pure mUser 

{-
main2 :: IO ()
main2 = scotty 3000 $ do
  middleware $ staticPolicy (addBase "/home/user/haskell/Scotty-Crud/")
  middleware $ checkRouteMiddleware
  get "/" $ do
    mUser <- getAuthUser
    html $ renderHtml $ homePage mUser
  get "/admin" $ do
    mUser <- getAuthUser
    case mUser of
      Nothing   -> redirect "/"
      Just user -> text $ "welcome " <> (TL.pack (user_email user))
  -}

authController :: ScottyM ()
authController = do
  get "/signup" $ do
    html $ renderHtml signUpPage
  get "/login" $ do
    mUser <- getAuthUser
    case mUser of
      Just _ -> redirect "/"
      Nothing -> html $ renderHtml loginPage

  post "/signupUser" $ do
    (email :: T.Text) <- formParam "email"
    (password :: T.Text) <- formParam "password"
    conn <- liftIO getConn
    userList <- liftIO $ (query conn "Select *FROM users where user_email = ?;" (Only email):: IO [User])
    case userList of
        [] -> do
            _ <- liftIO $ execute conn "insert into users (user_email,password) values (?,?);" (email,password)
            liftIO $ close conn
            text "user created"
        _  -> do
            liftIO $ close conn
            redirect "/login"

  post "/loginUser" $ do
    (email :: T.Text) <- formParam "email"
    (password' :: String) <- formParam "password"
    conn <- liftIO getConn
    userList <- liftIO $ (query conn "Select *FROM users where user_email = ?;" (Only email):: IO [User])
    case userList of
        [] -> text "sign up first :("
        [user]  -> do
            case (password user) == password' of
                True -> do
                    let key = hmacSecret . T.pack $ "hello cat"
                    let cs = mempty { -- mempty returns a default JWTClaimsSet
                                iss = stringOrURI . T.pack $ "Foo"
                                 , unregisteredClaims = ClaimsMap $ Map.fromList [(T.pack "user_id", (Number (fromIntegral $ user_id user)))]
                                }
                    let encryptedString = encodeSigned key mempty cs
                    setSimpleCookie "auth_token" encryptedString
                    liftIO $ close conn
                    redirect "/"
                False -> do
                    liftIO $ close conn
                    text "password is wrong!!"
        _    -> undefined
  get "/logout" $ do
    deleteCookie "auth_token"
    redirect "/"
