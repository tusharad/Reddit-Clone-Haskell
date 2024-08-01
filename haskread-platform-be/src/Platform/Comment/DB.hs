module Platform.Comment.DB (
    addCommentQ
  , fetchCommentByIDQ
  , deleteCommentQ
  , updateCommentQ
  , fetchCommentVoteQ
  , addCommentVoteQ
  , deleteCommentVoteQ
  , updateCommentVoteQ ) where

import Orville.PostgreSQL
import Platform.DB.Model
import Platform.DB.Table

addCommentQ :: (MonadOrville m) => CommentWrite -> m ()
addCommentQ = insertEntity commentTable

fetchCommentByIDQ :: MonadOrville m => CommentID -> m (Maybe CommentRead)
fetchCommentByIDQ = findEntity commentTable

deleteCommentQ :: MonadOrville m => CommentID -> m ()
deleteCommentQ = deleteEntity commentTable

updateCommentQ :: MonadOrville m => CommentID -> CommentWrite -> m ()
updateCommentQ = updateEntity commentTable

fetchCommentVoteQ :: MonadOrville m => CommentID -> UserID -> m (Maybe CommentVoteRead)
fetchCommentVoteQ cID uID = findEntity commentVoteTable (CommentVoteID uID cID)

addCommentVoteQ :: MonadOrville m => CommentVoteWrite -> m ()
addCommentVoteQ = insertEntity commentVoteTable

deleteCommentVoteQ :: MonadOrville m => CommentID -> UserID -> m ()
deleteCommentVoteQ cID uID = deleteEntity commentVoteTable (CommentVoteID uID cID)

updateCommentVoteQ :: MonadOrville m => CommentID -> UserID -> CommentVoteWrite -> m ()
updateCommentVoteQ cID uID = updateEntity commentVoteTable (CommentVoteID uID cID)