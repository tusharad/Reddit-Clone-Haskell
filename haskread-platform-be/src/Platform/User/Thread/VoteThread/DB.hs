module Platform.User.Thread.VoteThread.DB
  ( fetchThreadVoteQ,
    addThreadVoteQ,
    updateThreadVoteQ,
    deleteThreadVoteQ,
  )
where

import Orville.PostgreSQL
import Platform.DB.Model
import Platform.DB.Table

fetchThreadVoteQ :: (MonadOrville m) => UserID -> ThreadID -> m (Maybe ThreadVoteRead)
fetchThreadVoteQ uID tID = findEntity threadVoteTable (ThreadVoteID uID tID)

addThreadVoteQ :: (MonadOrville m) => ThreadVoteWrite -> m ()
addThreadVoteQ = insertEntity threadVoteTable

updateThreadVoteQ :: (MonadOrville m) => UserID -> ThreadID -> ThreadVoteWrite -> m ()
updateThreadVoteQ uID tID = updateEntity threadVoteTable (ThreadVoteID uID tID)

deleteThreadVoteQ :: (MonadOrville m) => UserID -> ThreadID -> m ()
deleteThreadVoteQ uID tID = deleteEntity threadVoteTable (ThreadVoteID uID tID)
