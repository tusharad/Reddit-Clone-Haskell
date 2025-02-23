{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Common.Request
  ( addThread
  , getUserCommentVotes
  , getUserThreadVotes
  , addComment
  , getCommentsByThreadId
  , getThreadByThreadId
  , verifyOtp
  , registerUser
  , loginUser
  , getUserInfo
  , voteThread
  , upvoteComment
  , downvoteComment
  , upvoteThread
  , downvoteThread
  , getAllThreads
  , getCommunityList
  , getAllThreadsBySearch
  , deleteThread
  , deleteComment
  , editThread
  , editComment
  , changePassword
  , deleteUser
  , getUserProfileImage
  ) where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import Platform.Common.Types

getUrl :: IO String
getUrl = pure "http://localhost:8085"

data RequestMethod = GET | POST | PATCH | DELETE | PUT
  deriving (Eq, Show)

data RequestOptions = RequestOptions
  { reqMethod :: RequestMethod
  , reqUrl :: String
  , mbReqHeaders :: Maybe RequestHeaders
  , mbQueryParams :: Maybe Query
  , mbReqBody :: Maybe BSL.ByteString
  , mbReqPath :: Maybe String
  , mbAuthToken :: Maybe BS.ByteString
  }
  deriving (Eq, Show)

doRequest :: RequestOptions -> IO (Either String BSL.ByteString)
doRequest RequestOptions {..} = do
  initialRequest <- parseRequest (reqUrl <> fromMaybe "" mbReqPath)
  let request =
        foldl
          (flip ($))
          initialRequest
          [ maybe Prelude.id setRequestHeaders mbReqHeaders
          , maybe Prelude.id setRequestQueryString mbQueryParams
          , maybe Prelude.id setRequestBodyLBS mbReqBody
          , maybe Prelude.id (\x -> addRequestHeader "Authorization" ("Bearer " <> x)) mbAuthToken
          , setRequestMethod (BS.pack $ show reqMethod)
          ]
  eResp <- try $ httpLbs request
  case eResp of
    Left err -> do 
        print ("err:" :: String, err)
        pure $ Left (show (err :: HttpException))
    Right resp -> do
      let respStatus = getResponseStatusCode resp
          respBody = getResponseBody resp
      if respStatus <= 299 && respStatus >= 200
        then return $ Right respBody
        else return $ Left (BSL.unpack respBody)

doRequestJSON :: (FromJSON b) => RequestOptions -> IO (Either String b)
doRequestJSON reqOpts = do
  eResp <- doRequest reqOpts
  case eResp of
    Left err -> return $ Left err
    Right respBody -> do
      let respBody' = if BSL.null respBody then "{}" else respBody
      case eitherDecode respBody' of
        Left err -> pure $ Left err
        Right res -> pure $ Right res

toPath :: [String] -> String
toPath = foldl (\acc x -> acc <> "/" <> x) ""

addThread :: CreateThreadData -> IO (Either String ())
addThread CreateThreadData {..} = do
  let myData =
        CreateThreadReqBody
          { threadTitleForCreate = titleForCreateThread
          , threadCommunityIDForCreate = communityId
          , threadDescriptionForCreate = if T.null content then Nothing else Just content
          }
  url <- getUrl
  case mToken of
    Nothing -> pure $ Left "Missing auth token, please login again"
    Just token -> do
      let reqOps =
            RequestOptions
              { reqMethod = POST
              , reqUrl = url
              , mbReqHeaders = Just [("Content-Type", "application/json")]
              , mbQueryParams = Nothing
              , mbReqBody = Just $ encode myData
              , mbReqPath = Just $ toPath ["api", "v1", "user", "thread", "create"]
              , mbAuthToken = Just $ TE.encodeUtf8 token
              }
      doRequestJSON reqOps

editComment :: Int -> Text -> Text -> IO (Either String ())
editComment cId token newContent = do
  let myData =
        UpdateCommentReqBody
          { commentContentForUpdate = newContent
          }
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = PUT
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode myData
          , mbReqPath = Just $ toPath ["api", "v1", "user", "comment", "update", show cId]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

editThread :: Text -> EditThreadData -> IO (Either String String)
editThread token EditThreadData {..} = do
  let myData =
        UpdateThreadReqBody
          { threadIDForUpdate = threadIdForEditThread
          , threadCommunityIDForUpdate = fromMaybe 6 communityIdForEditThread
          , threadTitleForUpdate = fromMaybe "" titleForEditThread
          , threadDescriptionForUpdate = descriptionForEditThread
          }
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = PUT
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode myData
          , mbReqPath = Just $ toPath ["api", "v1", "user", "thread", "update"]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

getUserCommentVotes :: Text -> [Int] -> IO (Either String FetchVoteComemntsForUserResponse)
getUserCommentVotes token threadIds = do
  let myData = threadIds
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = POST
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode myData
          , mbReqPath = Just $ toPath ["api", "v1", "user", "comment_votes"]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  eRes <- doRequestJSON reqOps :: IO (Either String [FetchVoteComments])
  case eRes of
    Left err -> pure $ Left err
    Right r -> do 
        pure $ Right $ FetchVoteComemntsForUserResponse r

getUserThreadVotes :: Text -> [Int] -> IO (Either String [(Int, Bool)])
getUserThreadVotes token threadIds = do
  let myData = FetchVoteThreadsForUserReq threadIds
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = POST
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode myData
          , mbReqPath = Just $ toPath ["api", "v1", "user", "thread_votes"]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

addComment :: AddCommentData -> IO (Either String ())
addComment AddCommentData {..} = do
  let myData =
        CreateCommentReqBody
          { threadIDForCommentCreate = threadId
          , commentContentForCreate = contentForAddComment
          , parentCommentIDForCreate = parentCommentIdForAddComment
          }
  let token = userToken
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = POST
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode myData
          , mbReqPath = Just $ toPath ["api", "v1", "user", "comment", "create"]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

getCommentsByThreadId :: Int -> IO (Either String FetchCommentsResponse)
getCommentsByThreadId tId = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = GET
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath = Just $ toPath ["api", "v1", "thread", "comment", show tId]
          , mbAuthToken = Nothing
          }
  doRequestJSON reqOps

getThreadByThreadId :: Int -> IO (Either String ThreadInfo)
getThreadByThreadId tId = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = GET
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath = Just $ toPath ["api", "v1", "thread", show tId]
          , mbAuthToken = Nothing
          }
  doRequestJSON reqOps

verifyOtp :: Int -> Int -> IO (Either String ())
verifyOtp newUserId otp = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = PUT
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath = Just $ toPath ["api", "v1", "user", "auth", "verify", show newUserId, show otp]
          , mbAuthToken = Nothing
          }
  doRequestJSON reqOps

registerUser :: Text -> Text -> Text -> Text -> IO (Either String RegisterUserResponse)
registerUser
  userNameForRegister
  emailForRegister
  passwordForRegister
  confirmPasswordForRegister = do
    let myData = RegisterUserBody {..}
    url <- getUrl
    let reqOps =
          RequestOptions
            { reqMethod = POST
            , reqUrl = url
            , mbReqHeaders = Just [("Content-Type", "application/json")]
            , mbQueryParams = Nothing
            , mbReqBody = Just $ encode myData
            , mbReqPath = Just $ toPath ["api", "v1", "user", "auth", "register"]
            , mbAuthToken = Nothing
            }
    doRequestJSON reqOps

loginUser :: Text -> Text -> IO (Either String LoginUserResponse)
loginUser email password = do
  let myData = LoginUserBody email password
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = POST
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode myData
          , mbReqPath = Just $ toPath ["api", "v1", "user", "auth", "login"]
          , mbAuthToken = Nothing
          }
  doRequestJSON reqOps

getUserInfo :: Text -> IO (Either String UserProfileResponse)
getUserInfo t = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = GET
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath = Just $ toPath ["api", "v1", "user", "profile"]
          , mbAuthToken = Just $ TE.encodeUtf8 t
          }
  doRequestJSON reqOps

voteThread :: Text -> Text -> Int -> IO (Either String ())
voteThread vote token tId = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = POST
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath = Just $ toPath ["api", "v1", "user", "thread", T.unpack vote, show tId]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

upvoteComment :: Maybe Text -> Int -> IO (Either String ())
upvoteComment Nothing _ = pure $ Left "Please login first"
upvoteComment (Just t) cId = voteComment True t cId

downvoteComment :: Maybe Text -> Int -> IO (Either String ())
downvoteComment Nothing _ = pure $ Left "Please login first"
downvoteComment (Just t) cId = voteComment False t cId

voteComment :: Bool -> Text -> Int -> IO (Either String ())
voteComment vote token cId = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = PUT
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath =
              Just $
                toPath
                  [ "api"
                  , "v1"
                  , "user"
                  , "comment"
                  , "vote"
                  , show cId
                  , map toLower (show vote)
                  ]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

upvoteThread :: Maybe Text -> Int -> IO (Either String ())
upvoteThread Nothing _ = pure $ Left "Please login first"
upvoteThread (Just t) tId = voteThread "upvote" t tId

downvoteThread :: Maybe Text -> Int -> IO (Either String ())
downvoteThread Nothing _ = pure $ Left "Please login first"
downvoteThread (Just t) tId = voteThread "downvote" t tId

getAllThreads ::
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  IO (Either String FetchAllThreadsResponse)
getAllThreads mbLimit mbOffset mbCommunityId mbUserId = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = GET
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Just [
                ("limit", Just . BS.pack $ show (fromMaybe 10 mbLimit))
              , maybe mempty (\x -> ("offset",Just . BS.pack $ show x)) mbOffset
              , maybe mempty (\x -> ("communityId",Just . BS.pack $ show x)) mbCommunityId
              , maybe mempty (\x -> ("userId",Just . BS.pack $ show x)) mbUserId
             ]
          , mbReqBody = Nothing
          , mbReqPath =
              Just $
                toPath
                  [ "api"
                  , "v1"
                  , "thread"
                  , "all" ]
          , mbAuthToken = Nothing
          }
  doRequestJSON reqOps

getCommunityList :: IO (Either String Communities)
getCommunityList = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = GET
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath =
              Just $
                toPath
                  [ "api", "v1", "community"]
          , mbAuthToken = Nothing
          }
  doRequestJSON reqOps

getAllThreadsBySearch :: Text -> IO (Either String FetchAllThreadsResponse)
getAllThreadsBySearch search = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = GET
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Just [("search_term", Just $ TE.encodeUtf8 search)]
          , mbReqBody = Nothing
          , mbReqPath =
              Just $
                toPath
                  [ "api", "v1", "user", "thread"]
          , mbAuthToken = Nothing
          }
  doRequestJSON reqOps

deleteThread :: Int -> Text -> IO (Either String ())
deleteThread threadId token = do 
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = DELETE
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath =
              Just $
                toPath
                  [ "api", "v1", "user", "thread", "delete", show threadId]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

deleteComment :: Int -> Text -> IO (Either String ())
deleteComment commentId token = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = DELETE
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath =
              Just $
                toPath
                  [ "api", "v1", "user", "comment", "delete", show commentId]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

changePassword :: Text -> ChangePasswordBody -> IO (Either String ())
changePassword token changePasswordBody = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = PUT
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode changePasswordBody
          , mbReqPath =
              Just $
                toPath
                  [ "api", "v1", "user", "profile", "change-password"]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

deleteUser :: Text -> DeleteUserBody -> IO (Either String ())
deleteUser token deleteUserBody = do
  url <- getUrl
  let reqOps =
        RequestOptions
          { reqMethod = PUT
          , reqUrl = url
          , mbReqHeaders = Just [("Content-Type", "application/json")]
          , mbQueryParams = Nothing
          , mbReqBody = Just $ encode deleteUserBody
          , mbReqPath =
              Just $
                toPath
                  [ "api", "v1", "user", "profile", "delete-account"]
          , mbAuthToken = Just $ TE.encodeUtf8 token
          }
  doRequestJSON reqOps

getUserProfileImage :: Int -> IO (Either String LBS.ByteString)
getUserProfileImage uId = do
  url <- getUrl
  doRequest RequestOptions {
    reqMethod = GET
          , reqUrl = url
          , mbReqHeaders = Nothing
          , mbQueryParams = Nothing
          , mbReqBody = Nothing
          , mbReqPath =
              Just $
                toPath
                  [ "api", "v1", "user", "profile-image", show uId]
          , mbAuthToken = Nothing
  }