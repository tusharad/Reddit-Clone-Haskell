module Platform.User.Thread.DB (
    fetchThreadByIDQ,
    addThreadQ,
    updateThreadQ,
    deleteThreadQ
) where

import Orville.PostgreSQL
import Platform.Common.AppM
import Platform.DB.Model
import Platform.DB.Table
import UnliftIO

fetchThreadByIDQ :: (MonadUnliftIO m) => ThreadID -> AppM m (Maybe ThreadRead)
fetchThreadByIDQ = findEntity threadTable

addThreadQ :: (MonadOrville m) => ThreadWrite -> m ()
addThreadQ = insertEntity threadTable

updateThreadQ :: MonadOrville m => ThreadID -> ThreadWrite -> m ()
updateThreadQ = updateEntity threadTable

deleteThreadQ :: MonadOrville m => ThreadID -> m ()
deleteThreadQ = deleteEntity threadTable