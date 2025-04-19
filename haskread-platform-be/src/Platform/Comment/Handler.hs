{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Comment.Handler
  ( createCommentH
  , deleteCommentH
  , updateCommentH
  , voteCommentH
  , fetchCommentsByThreadH
  , fetchVoteCommentsForUserH
  )
where

import Control.Monad (void, when)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Platform.Auth.Types
import Platform.Comment.DB
import Platform.Comment.Types
import Platform.Comment.Utils
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Platform.User.Thread.DB (fetchThreadByIDQ)
import Servant.Auth.Server
import UnliftIO

checkIfThreadExists :: (MonadUnliftIO m) => ThreadID -> AppM m ()
checkIfThreadExists tID = do
  mThread <- runQuery $ fetchThreadByIDQ tID
  when (isNothing mThread) $
    throw400Err "Thread does not exist!"

sanityCheckCommentContent :: (MonadUnliftIO m) => T.Text -> AppM m ()
sanityCheckCommentContent c = do
  when (T.null c) $
    throw400Err "Comment cannot be empty!"
  when (T.length c > 255) $
    throw400Err "Comment cannot be more than 255 characters!"

addComment ::
  (MonadUnliftIO m) =>
  UserID ->
  ThreadID ->
  T.Text ->
  Maybe CommentID ->
  AppM m CreateCommentResponse
addComment userID threadID comment mParentCommentID = do
  let commentWrite =
        Comment
          { commentID = ()
          , userIDForComment = userID
          , threadIDForComment = threadID
          , commentContent = comment
          , parentCommentID = mParentCommentID
          , createdAtForComment = ()
          , updatedAtForComment = ()
          }
  runQuery $ addCommentQ commentWrite
  return $ CreateCommentResponse "Comment added successfully!"

checkIfUserOwnsComment :: (MonadUnliftIO m) => CommentID -> UserID -> AppM m CommentRead
checkIfUserOwnsComment cID uID = do
  mComment <- runQuery $ fetchCommentByIDQ cID
  case mComment of
    Nothing -> throw400Err "Comment does not exist!"
    Just comment@Comment {..} ->
      if userIDForComment /= uID
        then
          throw400Err "You do not own this comment!"
        else pure comment

createCommentH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  CreateCommentReqBody ->
  AppM m CreateCommentResponse
createCommentH (Authenticated UserInfo {..}) CreateCommentReqBody {..} = do
  checkIfThreadExists threadIDForCommentCreate
  sanityCheckCommentContent commentContentForCreate
  addComment
    userIDForUserInfo
    threadIDForCommentCreate
    commentContentForCreate
    parentCommentIDForCreate
createCommentH _ _ = throw401Err "Please login first"

deleteCommentH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  CommentID ->
  AppM m DeleteCommentResponse
deleteCommentH (Authenticated UserInfo {..}) cID = do
  void $ checkIfUserOwnsComment cID userIDForUserInfo
  runQuery $ deleteCommentQ cID
  return $ DeleteCommentResponse "Comment deleted successfully!"
deleteCommentH _ _ = throw401Err "Please login first"

updateCommentH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  CommentID ->
  UpdateCommentReqBody ->
  AppM m UpdateCommentResponse
updateCommentH (Authenticated UserInfo {..}) commentID0 UpdateCommentReqBody {..} = do
  commentRead <- checkIfUserOwnsComment commentID0 userIDForUserInfo
  sanityCheckCommentContent commentContentForUpdate
  let commentWrite =
        commentRead
          { commentID = ()
          , userIDForComment = userIDForUserInfo
          , commentContent = commentContentForUpdate
          , createdAtForComment = ()
          , updatedAtForComment = ()
          }
  runQuery $ updateCommentQ commentID0 commentWrite
  return $ UpdateCommentResponse "Comment updated successfully!"
updateCommentH _ _ _ = throw401Err "Please login first"

voteCommentH ::
  (MonadUnliftIO m) =>
  AuthResult UserInfo ->
  CommentID ->
  Bool ->
  AppM m VoteCommentResponse
voteCommentH (Authenticated UserInfo {..}) cID vote = do
  mVoteComment <- fetchVoteComment cID userIDForUserInfo
  case mVoteComment of
    Nothing -> addVoteComment userIDForUserInfo cID vote
    Just voteComment ->
      if vote == commentVote voteComment
        then
          removeVoteComment userIDForUserInfo cID
        else updateVoteComment cID userIDForUserInfo vote
voteCommentH _ _ _ = throw401Err "Please login first"

fetchVoteComment ::
  (MonadUnliftIO m) =>
  CommentID ->
  UserID ->
  AppM m (Maybe CommentVoteRead)
fetchVoteComment cID uID = runQuery $ fetchCommentVoteQ cID uID

addVoteComment ::
  (MonadUnliftIO m) =>
  UserID ->
  CommentID ->
  Bool ->
  AppM m VoteCommentResponse
addVoteComment uID cID vote = do
  let commentVoteWrite =
        CommentVote
          { userIDForCommentVote = uID
          , commentIDForCommentVote = cID
          , commentVote = vote
          , createdAtForCommentVote = ()
          , updatedAtForCommentVote = ()
          }
  runQuery $ addCommentVoteQ commentVoteWrite
  return $ VoteCommentResponse "Vote added successfully!"

removeVoteComment ::
  (MonadUnliftIO m) =>
  UserID ->
  CommentID ->
  AppM m VoteCommentResponse
removeVoteComment uID cID = do
  runQuery $ deleteCommentVoteQ cID uID
  return $ VoteCommentResponse "Vote removed successfully!"

updateVoteComment ::
  (MonadUnliftIO m) =>
  CommentID ->
  UserID ->
  Bool ->
  AppM m VoteCommentResponse
updateVoteComment cID uID vote = do
  let commentVoteWrite =
        CommentVote
          { userIDForCommentVote = uID
          , commentIDForCommentVote = cID
          , commentVote = vote
          , createdAtForCommentVote = ()
          , updatedAtForCommentVote = ()
          }
  runQuery $ updateCommentVoteQ cID uID commentVoteWrite
  return $ VoteCommentResponse "Vote updated successfully!"

fetchCommentsByThreadH :: (MonadUnliftIO m) => ThreadID -> AppM m FetchCommentsResponse
fetchCommentsByThreadH threadID = do
  checkIfThreadExists threadID
  commentInfoList <- runQuery $ fetchCommentsByThreadQ threadID
  let res = buildNestedComments commentInfoList
  pure $ FetchCommentsResponse (length res) res

fetchVoteCommentsForUserH ::
  MonadUnliftIO m =>
  AuthResult UserInfo ->
  FetchVoteComemntsForUserReq ->
  AppM m FetchVoteCommentsForUserResponse
fetchVoteCommentsForUserH
  (Authenticated UserInfo {..})
  (FetchVoteComemntsForUserReq commentList) = do
    if null commentList
      then return $ FetchVoteCommentsForUserResponse []
      else do
        res <- fetchVoteCommentsByUser userIDForUserInfo commentList
        pure $
          FetchVoteCommentsForUserResponse $
            (\CommentVote {..} -> FetchVoteComments commentIDForCommentVote commentVote)
              <$> res
fetchVoteCommentsForUserH _ _ = throw401Err "Please login first"
