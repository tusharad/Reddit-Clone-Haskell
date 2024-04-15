{-# LANGUAGE OverloadedStrings #-}

module ScottyCrud.Auth where
import           Web.Scotty
import           Web.Scotty.Cookie
import           ScottyCrud.HTML
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           ScottyCrud.Common.Types
import qualified Data.Text as T
import           Web.JWT
import qualified Data.Map.Strict as Map
import           Data.Aeson
import           Data.Scientific (toBoundedInteger)
import           Data.Maybe (fromMaybe)
import           ScottyCrud.Query

{-
    scotty
  , yesod
  , servant
  , webapi
  , wai 
-}

jwtEncryptUserId :: Int -> T.Text
jwtEncryptUserId userId = do
  let key = hmacSecret . T.pack $ "hello cat"
  let cs = mempty { 
        iss = stringOrURI . T.pack $ "Foo"
        , unregisteredClaims = ClaimsMap $ Map.fromList [(T.pack "user_id", (Number $ fromIntegral userId))]
      }
  encodeSigned key mempty cs

fetchUserIdFromJWT res = Map.lookup "user_id" $ unClaimsMap $ unregisteredClaims res

checkIfUserIdIsValid :: Value -> IO (Maybe User)
checkIfUserIdIsValid (Number user_id) = do
    let user_id' = fromMaybe (0 :: Int) $ toBoundedInteger user_id
    userList <- getUserByIdQ user_id'
    case userList of
      [] -> pure $ Nothing
      [user] -> pure $ Just user

getAuthUser_ :: (Maybe T.Text) -> IO (Maybe User)
getAuthUser_ Nothing = pure Nothing
getAuthUser_ (Just encryptedData)= do
  let mRes = fmap claims $ decodeAndVerifySignature (toVerify . hmacSecret . T.pack $ "hello cat") encryptedData
      res = mRes >>= fetchUserIdFromJWT
  case res of
    Nothing -> pure Nothing
    Just r  -> checkIfUserIdIsValid r

getAuthUser :: ActionM (Maybe User)
getAuthUser = do
    mUserId <- getCookie "auth_token"
    mUser <- liftIO $ getAuthUser_ mUserId
    pure mUser 

authController :: ScottyM ()
authController = do
  get "/signup" $ do
    mMsg <- queryParamMaybe "message"
    html $ renderHtml $ signUpPage mMsg
  
  get "/login" $ do
    mUser <- getAuthUser
    mMsg <- queryParamMaybe "message"
    case mUser of
      Just _ -> redirect "/"
      Nothing -> html $ renderHtml $ loginPage mMsg

  post "/signupUser" $ do
    (email :: T.Text) <- formParam "email"
    (password :: T.Text) <- formParam "password"
    (confirmPassword :: T.Text) <- formParam "confirm_password"
    userList <- liftIO $ fetchUserByEmailQ email
    case userList of
        [] ->  if (password == confirmPassword) then redirect "/signup?message=password is did not matched" else (liftIO $ addUserQ email password) >> redirect "/"
        _  -> redirect "/login?message=user already exist"

  post "/loginUser" $ do
    (email :: T.Text) <- formParam "email"
    (password' :: String) <- formParam "password"
    conn <- liftIO getConn
    userList <- liftIO $ fetchUserByEmailQ email
    case userList of
        [] -> text "sign up first :("
        [user]  -> do
            case (password user) == password' of
                True -> do
                    let encryptedString = jwtEncryptUserId (user_id user)
                    setSimpleCookie "auth_token" encryptedString
                    redirect "/"
                False -> do
                    text "password is wrong!!"
  
  get "/logout" $ deleteCookie "auth_token" >> redirect "/"
