module Common.Utils where

import Prelude

import Affjax (printError)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (request, Request)
import Common.Types -- (BaseURL(..), Token(..), RequestOptions, RequestMethod(..), endpointCodec,Profile)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap,lmap)
import Data.Codec.Argonaut (JsonCodec,JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen.Store.Monad (class MonadStore, getStore,updateStore)
import Routing.Duplex (print)
import Store (Store, Action(..))
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Effect (Effect)
import Effect.Aff (Aff)
import Web.HTML (window)
import Data.Codec as Codec
import Data.Codec.Argonaut.Record as CAR
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Undefined (undefined)

mkRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case (rmap _.body response) of
    Left err -> (liftEffect $ log (printError err)) *> pure Nothing
    Right _ -> do
      pure $ hush $ rmap _.body response
 
mkAuthRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- getStore
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest baseUrl token opts
  pure $ hush $ map _.body response

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [RequestHeader "Authorization" $ "Bearer " <> t]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete b -> Tuple DELETE b

tokenKey = "token" :: String

safeHref :: forall r i. MyRoute -> HH.IProp (href :: String | r) i
safeHref = HP.href <<< append "#" <<< print myRoute

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ Token <$> str

writeToken :: Token -> Effect Unit
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken =
  removeItem tokenKey =<< localStorage =<< window

decode :: forall m a. MonadEffect m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> (log $ "failed decodig: " <> (CA.printJsonDecodeError err)) *> pure Nothing
  Right response -> pure (Just response)

verifyOtp :: forall m. 
    MonadStore Action Store m => 
    MonadAff m => OtpFields -> m (Either String Unit)
verifyOtp fields = do
    let method = Put Nothing
    mjson <- mkRequest { endpoint: VerifyOtp0 fields.userID  fields.otp, method }
    case mjson of
      Nothing -> pure $ Left "got nothing"
      Just _ -> pure $ Right unit
         
authenticate
  :: forall m a
   . MonadAff m
  => MonadStore Action Store m
  => (BaseURL -> a -> m (Either String (Tuple Token Profile)))
  -> a
  -> m (Maybe Profile)
authenticate req fields = do
  { baseUrl } <- getStore
  req baseUrl fields >>= case _ of
    Left err -> pure Nothing
    Right (Tuple token profile) -> do
      updateStore $ LoginUser profile
      pure (Just profile)

login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let
    method = Post $ Just $ Codec.encode loginCodec fields
  in
    requestUser baseUrl { endpoint: Login0, method }

register :: forall m. 
    MonadStore Action Store m => 
    MonadAff m => RegisterFields -> m (Either String Int)
register fields = do
    let method = Post $ Just $ Codec.encode registerCodec fields
    mjson <- mkRequest { endpoint: Register0, method }
    case mjson of
        Nothing -> pure $ Left "got nothing"
        Just registerResp -> do
           let eRes = decodeRegisterResp registerResp
           case eRes of
               Left _ -> pure $ Left "Decoding response failed"
               Right res -> pure $ Right res

decodeRegisterResp :: Json -> Either JsonDecodeError Int
decodeRegisterResp registerResp = do
    { userIDForRUR } <- Codec.decode decodeResp_ registerResp
    pure userIDForRUR
    where
      decodeResp_ =
          CAR.object "Register Response" { userIDForRUR : CA.int }

requestUser :: 
    forall m. MonadAff m => 
    BaseURL -> 
    RequestOptions -> 
    m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  eRes <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case eRes of
    Left e -> pure $ Left $ printError e
    Right v -> do 
      let eToken = lmap printJsonDecodeError $ decodeToken v.body
      case eToken of
        Left err -> do
          log $ "got error: " <> err 
          pure $ Left err
        Right token -> do
          _ <- liftEffect $ writeToken token
          mProfile <- liftAff $ getCurrentUser baseUrl
          case mProfile of
            Nothing -> pure $ Left ""
            Just profile ->
              pure (Right $ Tuple token profile)

decodeToken :: Json -> Either JsonDecodeError Token
decodeToken user = do
  { jwtToken } <- Codec.decode tokenCodec user
  pure $ Token jwtToken
  where
  tokenCodec =
     CAR.object "Token"
      { jwtToken: CA.string 
      }

getCurrentUser :: BaseURL -> Aff (Maybe Profile)
getCurrentUser baseUrl = do
    mToken <- liftEffect readToken
    case mToken of
        Nothing -> pure Nothing
        Just token -> do
           let requestOptions = { endpoint: UserByToken, method: Get }
           res <- request $ defaultRequest baseUrl (Just token) requestOptions
           let
               user :: Either String Profile
               user = case res of
                Left e ->
                    Left "error fetching request"
                Right v -> lmap printJsonDecodeError do
                    CA.decode profileCodec v.body
           pure $ hush user

createThread :: forall m. 
    MonadStore Action Store m => 
    MonadAff m => CreateThreadFields -> m (Maybe String)
createThread fields = do
    let method = Post $ Just $ Codec.encode createThreadCodec fields
    mjson <- mkAuthRequest { endpoint: CreateThread0 , method }
    case mjson of
      Nothing -> pure Nothing
      Just _ -> pure $ Just "All good"

changePassword :: forall m.
    MonadStore Action Store m =>
    MonadAff m => ChangePasswordFields -> m (Maybe String)
changePassword fields = do
    let method = Put $ Just $ Codec.encode changePasswordCodec fields
    mjson <- mkAuthRequest { endpoint: ChangePassword0, method }
    case mjson of
        Nothing -> pure Nothing
        Just _ -> pure $ Just "All good"

deleteThread :: forall m.
    MonadStore Action Store m =>
    MonadAff m => Int -> m (Maybe String)
deleteThread threadID = do
    let method = Delete Nothing
    mjson <- mkAuthRequest { endpoint : DeleteThread0 threadID, method }
    case mjson of
        Nothing -> pure Nothing
        Just _ -> pure $ Just "Thread deleted"

deleteUser :: forall m.
    MonadStore Action Store m =>
    MonadAff m => DeleteUserFields -> m (Maybe String)
deleteUser fields = do
    let method = Delete $ Just $ Codec.encode deleteUserCodec fields
    mjson <- mkAuthRequest { endpoint : DeleteUser0,method }
    case mjson of
        Nothing -> pure Nothing
        Just _ -> pure $ Just "User deleted"

getThread :: forall m.
    MonadStore Action Store m =>
    MonadAff m => Int -> m (Maybe Thread)
getThread threadID = undefined
