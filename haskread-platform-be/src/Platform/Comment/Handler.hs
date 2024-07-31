{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Platform.Comment.Handler (    
    createCommentH,
     deleteCommentH,
     updateCommentH
     ) where

import Platform.Common.AppM
import Platform.Comment.Types
import Platform.User.Thread.DB
import Platform.Auth.Types
import UnliftIO
import Control.Monad (void, when)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import Platform.Common.Utils
import Servant.Auth.Server
import Platform.DB.Model
import Data.Maybe (isNothing)
import Platform.Comment.DB

checkIfThreadExists :: MonadUnliftIO m => ThreadID -> AppM m ()
checkIfThreadExists tID = do
    eRes :: Either SomeException (Maybe ThreadRead) <- 
            try $ fetchThreadByIDQ tID
    case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right mThread -> when (isNothing mThread) $ 
            throw400Err "Thread does not exist!"

sanityCheckCommentContent :: MonadUnliftIO m => T.Text -> AppM m ()
sanityCheckCommentContent c = do
    when (T.null c) $ 
        throw400Err "Comment cannot be empty!"
    when (T.length c > 255) $
        throw400Err "Comment cannot be more than 255 characters!"

addComment :: MonadUnliftIO m => UserID -> ThreadID -> T.Text -> AppM m CreateCommentResponse
addComment userID threadID comment = do
    let commentWrite = Comment
            { commentID = ()
            , userIDForComment = userID
            , threadIDForComment = threadID
            , commentContent = comment
            , parentCommentID = Nothing
            , createdAtForComment = ()
            , updatedAtForComment = ()
            }
    (eRes :: Either SomeException ()) <- try $ addCommentQ commentWrite
    case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> return $ CreateCommentResponse "Comment added successfully!"

checkIfUserOwnsComment :: MonadUnliftIO m => CommentID -> UserID -> AppM m CommentRead
checkIfUserOwnsComment cID uID = do
    eRes :: Either SomeException (Maybe CommentRead) <- 
            try $ fetchCommentByIDQ cID
    case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right mComment -> case mComment of
            Nothing -> throw400Err "Comment does not exist!"
            Just comment@Comment{..} -> if (userIDForComment /= uID) then 
                throw400Err "You do not own this comment!"
            else pure comment

createCommentH :: (MonadUnliftIO m) => AuthResult UserInfo -> CreateCommentReqBody -> AppM m CreateCommentResponse
createCommentH (Authenticated UserInfo{..}) CreateCommentReqBody{..} = do
    checkIfThreadExists threadIDForCommentCreate
    sanityCheckCommentContent commentContentForCreate
    addComment userIDForUserInfo threadIDForCommentCreate commentContentForCreate
createCommentH _ _ = throw401Err "Please login first"

deleteCommentH :: MonadUnliftIO m => AuthResult UserInfo -> CommentID -> AppM m DeleteCommentResponse
deleteCommentH (Authenticated UserInfo{..}) cID = do
    void $ checkIfUserOwnsComment cID userIDForUserInfo
    (eRes :: Either SomeException ()) <- try $ deleteCommentQ cID
    case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> return $ DeleteCommentResponse "Comment deleted successfully!"
deleteCommentH _ _ = throw401Err "Please login first"

updateCommentH :: MonadUnliftIO m => AuthResult UserInfo -> CommentID -> UpdateCommentReqBody -> AppM m UpdateCommentResponse
updateCommentH (Authenticated UserInfo{..}) commentID0 UpdateCommentReqBody{..} = do
    commentRead <- checkIfUserOwnsComment commentID0 userIDForUserInfo
    sanityCheckCommentContent commentContentForUpdate
    let commentWrite = commentRead
            { commentID = ()
            , userIDForComment = userIDForUserInfo
            , commentContent = commentContentForUpdate
            , createdAtForComment = ()
            , updatedAtForComment = ()
            }
    (eRes :: Either SomeException ()) <- try $ updateCommentQ commentID0 commentWrite
    case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ -> return $ UpdateCommentResponse "Comment updated successfully!"
updateCommentH _ _ _ = throw401Err "Please login first"