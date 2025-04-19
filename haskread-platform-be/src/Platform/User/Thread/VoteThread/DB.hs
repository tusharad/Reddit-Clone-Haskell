module Platform.User.Thread.VoteThread.DB
  ( fetchThreadVoteQ
  , addThreadVoteQ
  , updateThreadVoteQ
  , deleteThreadVoteQ
  , fetchVoteThreadsByUser
  )
where

import Data.Coerce (coerce)
import Data.List.NonEmpty
import Orville.PostgreSQL
import Orville.PostgreSQL.Expr
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import Platform.Common.AppM (AppM)
import Platform.Common.Utils (runQuery)
import Platform.DB.Marshaller (threadIDField, userIDField)
import Platform.DB.Model
import Platform.DB.Table
import UnliftIO

fetchThreadVoteQ :: (MonadOrville m) => UserID -> ThreadID -> m (Maybe ThreadVoteRead)
fetchThreadVoteQ uID tID = findEntity threadVoteTable (ThreadVoteID uID tID)

addThreadVoteQ :: (MonadOrville m) => ThreadVoteWrite -> m ()
addThreadVoteQ = insertEntity threadVoteTable

updateThreadVoteQ :: (MonadOrville m) => UserID -> ThreadID -> ThreadVoteWrite -> m ()
updateThreadVoteQ uID tID = updateEntity threadVoteTable (ThreadVoteID uID tID)

deleteThreadVoteQ :: (MonadOrville m) => UserID -> ThreadID -> m ()
deleteThreadVoteQ uID tID = deleteEntity threadVoteTable (ThreadVoteID uID tID)

fetchVoteThreadsByUser ::
  MonadUnliftIO m =>
  UserID ->
  [ThreadID] ->
  AppM m [ThreadVoteRead]
fetchVoteThreadsByUser u t =
  runQuery $
    findEntitiesBy
      threadVoteTable
      ( where_
          ( fieldColumnReference userIDField
              `equals` valueExpression
                (SqlValue.fromInt32 $ coerce u)
              .&& valueIn
                (fieldColumnReference threadIDField)
                (fromList (valueExpression . SqlValue.fromInt32 . coerce <$> t))
          )
      )

{-
select *from vote_thread where user_id = 22 and thread_id in (10,11,12);
-}
