module Platform.Comment.DB (
    addCommentQ,fetchCommentByIDQ,deleteCommentQ,updateCommentQ) where

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