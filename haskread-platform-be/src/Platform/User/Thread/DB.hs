module Platform.User.Thread.DB
  ( fetchThreadByIDQ,
    addThreadQ,
    updateThreadQ,
    deleteThreadQ,
    fetchAllThreadsQ,
  )
where

import Orville.PostgreSQL
import Platform.Common.Utils (threadToThreadInfo)
import Platform.DB.Model
import Platform.DB.Table

fetchThreadByIDQ :: (MonadOrville m) => ThreadID -> m (Maybe ThreadRead)
fetchThreadByIDQ = findEntity threadTable

addThreadQ :: (MonadOrville m) => ThreadWrite -> m ()
addThreadQ = insertEntity threadTable

updateThreadQ :: (MonadOrville m) => ThreadID -> ThreadWrite -> m ()
updateThreadQ = updateEntity threadTable

deleteThreadQ :: (MonadOrville m) => ThreadID -> m ()
deleteThreadQ = deleteEntity threadTable

{-
select thread_title,thread_description from thread;
-}
fetchAllThreadsQ :: (MonadOrville m) => m [ThreadInfo]
fetchAllThreadsQ = fmap threadToThreadInfo <$> findEntitiesBy threadTable emptySelectOptions
