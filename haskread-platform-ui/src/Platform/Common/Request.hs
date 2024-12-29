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
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Req
import Platform.Common.Types
import Platform.Common.Utils (toText)

addThread :: CreateThreadData -> IO (Maybe String)
addThread CreateThreadData {..} = do
  let myData =
        CreateThreadReqBody
          { threadTitleForCreate = titleForCreateThread
          , threadCommunityIDForCreate = communityId
          , threadDescriptionForCreate = if T.null content then Nothing else Just content
          }
  case mToken of
    Nothing -> pure Nothing
    Just token -> do
      eRes ::
        Either HttpException (Maybe String) <-
        try $ runReq defaultHttpConfig $ do
          bsResp <-
            req
              POST
              ( http
                  "localhost"
                  /: "api"
                  /: "v1"
                  /: "user"
                  /: "thread"
                  /: "create"
              )
              (ReqBodyJson myData)
              bsResponse
              $ header "Authorization" ("Bearer " <> TE.encodeUtf8 token)
                <> port 8085
          if responseStatusCode bsResp == 200
            then
              pure $ Just "All good"
            else do
              liftIO $ print $ responseStatusCode bsResp
              pure Nothing
      print eRes
      case eRes of
        Left _ -> pure Nothing
        Right r -> pure r

getUserCommentVotes :: Text -> [Int] -> IO (Maybe [(Int, Bool)])
getUserCommentVotes token threadIds = do
  let myData = threadIds
  eRes ::
    Either HttpException (Maybe [FetchVoteComments]) <-
    try $ runReq defaultHttpConfig $ do
      jsResp <-
        req
          POST
          ( http
              "localhost"
              /: "api"
              /: "v1"
              /: "user"
              /: "comment_votes"
          )
          (ReqBodyJson myData)
          jsonResponse
          $ header "Authorization" ("Bearer " <> TE.encodeUtf8 token)
            <> port 8085
      if responseStatusCode jsResp == 200
        then
          pure $ responseBody jsResp
        else do
          liftIO $ print $ responseStatusCode jsResp
          pure Nothing
  print eRes
  case eRes of
    Left _ -> pure Nothing
    Right r ->
      pure $
        fmap
          (map (\FetchVoteComments {..} -> (commentIDForFetchVote, isUpvote)))
          r

getUserThreadVotes :: Text -> [Int] -> IO (Maybe [(Int, Bool)])
getUserThreadVotes token threadIds = do
  let myData = FetchVoteThreadsForUserReq threadIds
  eRes ::
    Either HttpException (Maybe [(Int, Bool)]) <-
    try $ runReq defaultHttpConfig $ do
      jsResp <-
        req
          POST
          ( http
              "localhost"
              /: "api"
              /: "v1"
              /: "user"
              /: "thread_votes"
          )
          (ReqBodyJson myData)
          jsonResponse
          $ header "Authorization" ("Bearer " <> TE.encodeUtf8 token)
            <> port 8085
      if responseStatusCode jsResp == 200
        then
          pure $ responseBody jsResp
        else do
          liftIO $ print $ responseStatusCode jsResp
          pure Nothing
  print eRes
  case eRes of
    Left _ -> pure Nothing
    Right r -> pure r

addComment :: AddCommentData -> IO (Maybe String)
addComment AddCommentData {..} = do
  let myData =
        CreateCommentReqBody
          { threadIDForCommentCreate = threadId
          , commentContentForCreate = contentForAddComment
          , parentCommentIDForCreate = parentCommentIdForAddComment
          }
  let token = userToken
  eRes ::
    Either HttpException (Maybe String) <-
    try $ runReq defaultHttpConfig $ do
      bsResp <-
        req
          POST
          ( http
              "localhost"
              /: "api"
              /: "v1"
              /: "user"
              /: "comment"
              /: "create"
          )
          (ReqBodyJson myData)
          bsResponse
          $ header "Authorization" ("Bearer " <> TE.encodeUtf8 token)
            <> port 8085
      if responseStatusCode bsResp == 200
        then
          pure $ Just "All good"
        else do
          liftIO $ print $ responseStatusCode bsResp
          pure Nothing
  print eRes
  case eRes of
    Left _ -> pure Nothing
    Right r -> pure r

getCommentsByThreadId :: Int -> IO (Maybe FetchCommentsResponse)
getCommentsByThreadId tId = do
  eRes ::
    Either HttpException (Maybe FetchCommentsResponse) <-
    try $ runReq defaultHttpConfig $ do
      jsResp <-
        req
          GET
          ( http
              "localhost"
              /: "api"
              /: "v1"
              /: "thread"
              /: "comment"
              /: T.pack (show tId)
          )
          NoReqBody
          jsonResponse
          $ port 8085
      if responseStatusCode jsResp == 200
        then
          pure $ Just (responseBody jsResp :: FetchCommentsResponse)
        else pure Nothing
  case eRes of
    Left _ -> pure Nothing
    Right r -> pure r

getThreadByThreadId :: Int -> IO (Maybe ThreadInfo)
getThreadByThreadId tId = do
  eRes ::
    Either HttpException (Maybe ThreadInfo) <-
    try $ runReq defaultHttpConfig $ do
      jsResp <-
        req
          GET
          ( http
              "localhost"
              /: "api"
              /: "v1"
              /: "thread"
              /: T.pack (show tId)
          )
          NoReqBody
          jsonResponse
          $ port 8085
      if responseStatusCode jsResp == 200
        then
          pure $ Just (responseBody jsResp :: ThreadInfo)
        else pure Nothing
  case eRes of
    Left _ -> pure Nothing
    Right r -> pure r

verifyOtp :: Int -> Int -> IO (Maybe String)
verifyOtp newUserId otp = do
  eRes ::
    Either HttpException (Maybe String) <-
    try $ runReq defaultHttpConfig $ do
      bsResp <-
        req
          PUT
          ( http
              "localhost"
              /: "api"
              /: "v1"
              /: "user"
              /: "auth"
              /: "verify"
              /: T.pack (show newUserId)
              /: T.pack (show otp)
          )
          NoReqBody
          bsResponse
          $ port 8085
      if responseStatusCode bsResp == 200
        then
          pure $ Just "all good"
        else pure Nothing
  case eRes of
    Left _ -> pure Nothing
    Right _ -> pure $ Just "all good"

registerUser :: Text -> Text -> Text -> Text -> IO (Maybe RegisterUserResponse)
registerUser
  userNameForRegister
  emailForRegister
  passwordForRegister
  confirmPasswordForRegister = do
    let myData = RegisterUserBody {..}
    eRes ::
      Either HttpException (Maybe RegisterUserResponse) <-
      try $ runReq defaultHttpConfig $ do
        jsResp <-
          req
            POST
            (http "localhost" /: "api" /: "v1" /: "user" /: "auth" /: "register")
            (ReqBodyJson myData)
            jsonResponse
            $ port 8085
        if responseStatusCode jsResp == 200
          then
            pure $ Just (responseBody jsResp :: RegisterUserResponse)
          else pure Nothing
    case eRes of
      Left _ -> pure Nothing
      Right r -> pure r

loginUser :: Text -> Text -> IO (Maybe LoginUserResponse)
loginUser email password = do
  let myData = LoginUserBody email password
  eRes :: Either HttpException (Maybe LoginUserResponse) <- try $ runReq defaultHttpConfig $ do
    jsResp <-
      req
        POST
        (http "localhost" /: "api" /: "v1" /: "user" /: "auth" /: "login")
        (ReqBodyJson myData)
        jsonResponse
        $ port 8085
    if responseStatusCode jsResp == 200
      then
        pure $ Just (responseBody jsResp :: LoginUserResponse)
      else pure Nothing
  case eRes of
    Left _ -> pure Nothing
    Right r -> pure r

getUserInfo :: Text -> IO (Maybe UserProfileResponse)
getUserInfo t = do
  eRes :: Either HttpException (Maybe UserProfileResponse) <- try $ runReq defaultHttpConfig $ do
    jsResp <-
      req
        GET
        (http "localhost" /: "api" /: "v1" /: "user" /: "profile")
        NoReqBody
        jsonResponse
        $ header "Authorization" ("Bearer " <> TE.encodeUtf8 t)
          <> port 8085
    if responseStatusCode jsResp == 200
      then
        pure $ Just (responseBody jsResp :: UserProfileResponse)
      else pure Nothing
  case eRes of
    Left _ -> pure Nothing
    Right r -> pure r

voteThread :: Text -> Text -> Int -> IO ()
voteThread vote t tId = runReq defaultHttpConfig $ do
  _ <-
    req
      POST
      (http "localhost" /: "api" /: "v1" /: "user" /: "thread" /: vote /: T.pack (show tId))
      NoReqBody
      bsResponse
      $ header "Authorization" ("Bearer " <> TE.encodeUtf8 t)
        <> port 8085
  pure ()

upvoteComment :: Maybe Text -> Int -> IO ()
upvoteComment Nothing _ = pure ()
upvoteComment (Just t) cId = voteComment True t cId

downvoteComment :: Maybe Text -> Int -> IO ()
downvoteComment Nothing _ = pure ()
downvoteComment (Just t) cId = voteComment False t cId

voteComment :: Bool -> Text -> Int -> IO ()
voteComment vote t cId = runReq defaultHttpConfig $ do
  _ <-
    req
      PUT
      ( http
          "localhost"
          /: "api"
          /: "v1"
          /: "user"
          /: "comment"
          /: "vote"
          /: T.pack (show cId)
          /: T.toLower (T.pack (show vote))
      )
      NoReqBody
      bsResponse
      $ header "Authorization" ("Bearer " <> TE.encodeUtf8 t)
        <> port 8085
  pure ()

upvoteThread :: Maybe Text -> Int -> IO ()
upvoteThread Nothing _ = pure ()
upvoteThread (Just t) tId = voteThread "upvote" t tId

downvoteThread :: Maybe Text -> Int -> IO ()
downvoteThread Nothing _ = pure ()
downvoteThread (Just t) tId = voteThread "downvote" t tId

getAllThreads ::
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  IO FetchAllThreadsResponse
getAllThreads mbLimit mbOffset mbCommunityId mbUserId = runReq defaultHttpConfig $ do
  jsonResp <-
    req
      GET
      (http "localhost" /: "api" /: "v1" /: "thread" /: "all")
      NoReqBody
      jsonResponse
      $ ("limit" =: fromMaybe 10 mbLimit)
        <> maybe mempty ("offset" =:) mbOffset
        <> maybe mempty ("communityId" =:) mbCommunityId
        <> maybe mempty ("userId" =:) mbUserId
        <> port 8085
  pure (responseBody jsonResp :: FetchAllThreadsResponse)

getCommunityList :: IO [CommunityC]
getCommunityList = runReq defaultHttpConfig $ do
  jsonResp <- req
    GET
    (http "localhost" /: "api" /: "v1" /: "community")
    NoReqBody
    jsonResponse
    $ do
      port 8085
  let (Communities res) = (responseBody jsonResp :: Communities)
  pure res

getAllThreadsBySearch :: Text -> IO (Maybe FetchAllThreadsResponse)
getAllThreadsBySearch search = do
  eRes ::
    Either HttpException (Maybe FetchAllThreadsResponse) <- try $ runReq defaultHttpConfig $ do
    jsonResp <-
      req
        GET
        (http "localhost" /: "api" /: "v1" /: "user" /: "thread")
        NoReqBody
        jsonResponse
        $ "search_term" =: search
          <> port 8085
    pure $ Just (responseBody jsonResp :: FetchAllThreadsResponse)
  case eRes of
    Left _ -> pure Nothing
    Right r -> pure r

deleteThread :: Int -> Text -> IO (Maybe String)
deleteThread threadId token = do
  eRes ::
    Either HttpException (Maybe String) <- try $ runReq defaultHttpConfig $ do
    bResponse <-
      req
        DELETE
        (http "localhost" /: "api" /: "v1" /: "user" /: "thread" /: "delete" /: toText threadId)
        NoReqBody
        bsResponse
        $ 
          header "Authorization" ("Bearer " <> TE.encodeUtf8 token)
          <> port 8085
    if responseStatusCode bResponse == 200 then
      pure $ Just "all good"
    else do 
      liftIO $ print (responseBody bResponse)
      pure Nothing
  case eRes of
    Left e -> do 
      print e
      pure Nothing
    Right r -> pure r


deleteComment :: Int -> Text -> IO (Maybe String)
deleteComment commentId token = do
  eRes ::
    Either HttpException (Maybe String) <- try $ runReq defaultHttpConfig $ do
    bResponse <-
      req
        DELETE
        (http "localhost" /: "api" /: "v1" /: "user" /: "comment" /: "delete" /: toText commentId)
        NoReqBody
        bsResponse
        $ 
          header "Authorization" ("Bearer " <> TE.encodeUtf8 token)
          <> port 8085
    if responseStatusCode bResponse == 200 then
      pure $ Just "all good"
    else do 
      liftIO $ print (responseBody bResponse)
      pure Nothing
  case eRes of
    Left e -> do 
      print e
      pure Nothing
    Right r -> pure r
