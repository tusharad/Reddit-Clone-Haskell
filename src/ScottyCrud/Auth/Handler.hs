{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ScottyCrud.Auth.Handler where

import qualified Data.Text as T
import           Data.Scientific (toBoundedInteger)
import           Data.Maybe (fromMaybe)
import           Data.Password.Bcrypt
import           Web.JWT
import           ScottyCrud.Common.Types hiding (MyData(..))
import qualified ScottyCrud.Common.Types as CT
import           ScottyCrud.Query
import           Web.Scotty.Cookie
import           ScottyCrud.HTML.Auth
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Data.Aeson
import qualified Data.Map.Strict as Map
import           Data.UUID.V4
import           Data.UUID
import           Network.HTTP.Req as Req
import           Web.Scotty.Trans as ST
import           Control.Monad.Reader
import qualified  Data.ByteString.Char8 as BS

sendVerificationMail :: Int -> T.Text -> T.Text -> Int -> T.Text -> String -> IO ()
sendVerificationMail uid email hashedToken portNum fromEmail apiToken = Req.runReq Req.defaultHttpConfig $ do

  let myData = CT.MyData {
    from = From fromEmail,
    to = [Email email],
    subject = "Test mail!",
    CT.text = "click here to verify your account\n http://localhost:" <> T.pack (show portNum) <> "/verify_email/" <> T.pack (show uid) <> "?token=" <> hashedToken
  }

  bs <- Req.req POST (https "api.mailersend.com" /: "v1" /: "email") (ReqBodyJson myData) bsResponse $ do
    oAuth2Bearer (BS.pack apiToken)
    <> Req.header "X-Requested-With" "XMLHttpRequest"
    <> Req.header "Content-Type" "application/json"
  liftIO $ print (responseBody bs)

getVerifyEmail :: ActionT AppM ()
getVerifyEmail = do
  uid <- pathParam "uid"
  hashedtoken <- ST.queryParam "token"
  userList <- lift $ fetchUserTokenQ uid
  case userList of
    Nothing     -> redirect "/"
    Just tokenVal -> do
      case checkPassword (mkPassword tokenVal) (PasswordHash hashedtoken) of
        PasswordCheckSuccess -> lift (verifyUserQ uid) >> redirect "/login"
        _                    -> redirect "/?message=verification failed"

getSignupR :: ActionT AppM ()
getSignupR = do
    mMsg <- queryParamMaybe "message"
    html $ renderHtml (signUpPage mMsg)

getLoginR :: ActionT AppM ()
getLoginR = do
    mUser <- getAuthUser
    mMsg <- queryParamMaybe "message"
    case mUser of
      Just _ -> redirect "/"
      Nothing -> html $ renderHtml $ loginPage mMsg

postSignupUserR :: ActionT AppM ()
postSignupUserR = do
    (userName :: T.Text)        <- formParam "userName"
    (email :: T.Text)           <- formParam "email"
    (password :: T.Text)        <- formParam "password"
    (confirmPassword :: T.Text) <- formParam "confirm_password"
    userNameList                <- lift $ fetchUserByUserNameQ userName
    userList                    <- lift $ fetchUserByEmailQ email
    case userList of
        [] ->  do
          case userNameList of
            [] -> if password /= confirmPassword then redirect "/signup?message=password is did not matched" else do
              tokenVal <- liftIO nextRandom
              uid <- lift (addUserQ email password userName (toText tokenVal))
              hashedToken <- hashPassword $ mkPassword (toText tokenVal)
              p <- asks ScottyCrud.Common.Types.port
              apiToken <- asks ScottyCrud.Common.Types.mailerSendAPIToken
              fromEmail <- asks ScottyCrud.Common.Types.mailerSendFromEmail
              liftIO $ sendVerificationMail uid email (unPasswordHash hashedToken) p fromEmail apiToken
              redirect "/"
            _ -> redirect "/signup?message=user name is taken"
        _  -> redirect "/login?message=user already exist"

postLoginUserR :: ActionT AppM ()
postLoginUserR = do
    (email :: T.Text)     <- formParam "email"
    (password_ :: String) <- formParam "password"
    userList              <- lift $ fetchUserByEmailQ email
    case userList of
        [] -> text "sign up first :("
        [user]  -> do
            let hashedPassword = password user
            case checkPassword (mkPassword (T.pack password_)) (PasswordHash (T.pack hashedPassword)) of
              PasswordCheckSuccess -> do
                if not $ isVerified user then text "please verify account first" else do
                  setSimpleCookie "auth_token" (jwtEncryptUserId (user_id user)) >> redirect "/"
              _                    -> text "password is wrong!!"
        _       -> undefined

getLogoutR :: ActionT AppM ()
getLogoutR = deleteCookie "auth_token" >> redirect "/"

putPasswordResetR :: ActionT AppM ()
putPasswordResetR = do
    mUser <- getAuthUser
    case mUser of
      Nothing -> redirect "/?message=user not valid"
      Just user -> do
        currentPassword <- formParam "current_password"
        newPassword  <- formParam "new_password"
        confirmPassword <- formParam "confirm_password"
        if newPassword /= confirmPassword then redirect "/?message=passwords not matching"
        else do
          case checkPassword (mkPassword currentPassword) (PasswordHash (T.pack $ password user)) of
            PasswordCheckSuccess -> lift (updateUserPasswordQ (user_id user) newPassword) >> redirect "/?message=password has been updated"
            _                    -> redirect "/?message=password is wrong"

jwtEncryptUserId :: Int -> T.Text
jwtEncryptUserId userId = do
  let key = hmacSecret . T.pack $ "hello cat"
  let cs = mempty {
          iss = stringOrURI . T.pack $ "Foo"
        , unregisteredClaims = ClaimsMap $ Map.fromList [(T.pack "user_id", Number $ fromIntegral userId)]
      }
  encodeSigned key mempty cs

fetchUserIdFromJWT :: JWTClaimsSet -> Maybe Value
fetchUserIdFromJWT res = Map.lookup "user_id" $ unClaimsMap $ unregisteredClaims res

checkIfUserIdIsValid :: Value -> AppM (Maybe User)
checkIfUserIdIsValid (Number user_id) = do
    let user_id' = fromMaybe (0 :: Int) $ toBoundedInteger user_id
    userList <- getUserByIdQ user_id'
    case userList of
      []     -> pure Nothing
      [user] -> pure $ Just user
      _      -> undefined
checkIfUserIdIsValid _ = undefined

getAuthUser_ :: Maybe T.Text -> AppM (Maybe User)
getAuthUser_ Nothing = pure Nothing
getAuthUser_ (Just encryptedData)= do
  let mRes = claims <$> decodeAndVerifySignature (toVerify . hmacSecret . T.pack $ "hello cat") encryptedData
      res = mRes >>= fetchUserIdFromJWT
  case res of
    Nothing -> pure Nothing
    Just r  -> checkIfUserIdIsValid r

getAuthUser :: ActionT AppM (Maybe User)
getAuthUser = do
    mUserId <- getCookie "auth_token"
    lift $ getAuthUser_ mUserId
