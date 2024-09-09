module Platform.User.Thread.DB
  ( fetchThreadByIDQ,
    addThreadQ,
    updateThreadQ,
    deleteThreadQ,
    fetchAllThreadsQ,
    fetchThreadInfoQ,
    fetchThreadInfoByIDQ,
  )
where

-- import qualified Orville.PostgreSQL.Expr as Expr

import Data.List.NonEmpty
import Orville.PostgreSQL
import Orville.PostgreSQL.Expr hiding (tableName)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import Platform.Orville.Helper

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
fetchAllThreadsQ = undefined

{-
 SELECT t.thread_id,
       t.thread_title,
       thread_description,
       t.created_at,
       u.user_id,
       user_name,
       c.community_id,
       c.community_name,
       upvote_count,
       downvote_count,
       comment_count
FROM   THREAD t
       join users u
         ON t.user_id = u.user_id
       join community c
         ON t.community_id = c.community_id
       left join (SELECT Count(CASE
                                 WHEN vote = TRUE THEN 1
                                 WHEN vote = FALSE THEN NULL
                               END) AS upvote_count,
                         Count(CASE
                                 WHEN vote = TRUE THEN NULL
                                 WHEN vote = FALSE THEN 1
                               END) AS downvote_count,
                         thread_id
                  FROM   vote_thread
                  GROUP  BY thread_id) AS s
              ON t.thread_id = s.thread_id;
-}

whereThreadIdIs :: ThreadID -> WhereClause
whereThreadIdIs (ThreadID tID) =
  whereClause $
    columnReference (fieldColumnName (Just (stringToAliasName "t")) threadIDField)
      `equals` valueExpression (SqlValue.fromInt32 tID)

fetchThreadInfoByIDQ :: (MonadOrville m) => ThreadID -> m (Maybe ThreadInfo)
fetchThreadInfoByIDQ tID = do
  res <-
    executeAndDecode
      SelectQuery
      (fetchThreadInfoExpr (Just $ whereThreadIdIs tID) Nothing Nothing)
      (annotateSqlMarshallerEmptyAnnotation threadInfoMarshaller)
  case res of
    [] -> pure Nothing
    (x : _) -> pure $ Just x

fetchThreadInfoQ :: (MonadOrville m) => Int -> Int -> m [ThreadInfo]
fetchThreadInfoQ limit offset =
  executeAndDecode
    SelectQuery
    (fetchThreadInfoExpr Nothing (Just (limitExpr limit)) (Just (offsetExpr offset)))
    (annotateSqlMarshallerEmptyAnnotation threadInfoMarshaller)

fetchThreadInfoExpr :: Maybe WhereClause -> Maybe LimitExpr -> Maybe OffsetExpr -> QueryExpr
fetchThreadInfoExpr wClause lClause oClause =
  queryExpr selectClauseDefault selectedColumns (Just (fromTable wClause lClause oClause))
  where
    selectedColumns =
      selectColumns
        [ fieldColumnName (Just (stringToAliasName "t")) threadIDField,
          fieldColumnName (Just (stringToAliasName "t")) threadTitleField,
          fieldColumnName (Just (stringToAliasName "t")) threadDescriptionField,
          fieldColumnName (Just (stringToAliasName "t")) createdAtField,
          fieldColumnName (Just (stringToAliasName "u")) userIDField,
          fieldColumnName (Just (stringToAliasName "u")) userNameField,
          fieldColumnName (Just (stringToAliasName "c")) communityIDField,
          fieldColumnName (Just (stringToAliasName "c")) communityNameField,
          fieldColumnName Nothing upvoteCountField,
          fieldColumnName Nothing downvoteCountField,
          fieldColumnName Nothing commentCountField
        ]
    threadIDColumnName = fieldColumnName Nothing threadIDField
    threadTableName = tableFromItemWithAlias (stringToAliasExpr "t") (tableName threadTable)
    userTableName = tableFromItemWithAlias (stringToAliasExpr "u") (tableName userTable)
    communityTableName =
      tableFromItemWithAlias
        (stringToAliasExpr "c")
        (tableName communityTable)
    userIDConstraint =
      columnReference (fieldColumnName (Just (stringToAliasName "u")) userIDField)
        `equals` columnReference (fieldColumnName (Just (stringToAliasName "t")) userIDField)
    communityIDConstraint =
      columnReference (fieldColumnName (Just (stringToAliasName "c")) communityIDField)
        `equals` columnReference (fieldColumnName (Just (stringToAliasName "t")) communityIDField)
    commentConstraint =
      columnReference (fieldColumnName (Just (stringToAliasName "comm")) threadIDField)
        `equals` columnReference (fieldColumnName (Just (stringToAliasName "t")) threadIDField)
    voteCountTable =
      subQueryAsFromItemExpr (stringToAliasExpr "s") voteCountExpr
    voteCountExpr =
      queryExpr selectClauseDefault voteCountSelectList (Just fromVoteThreadTable)
    fromVoteThreadTable =
      tableExpr
        (tableFromItem (tableName threadVoteTable))
        Nothing
        (Just groupByThreadID)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
    groupByThreadID =
      groupByClause (groupByColumnsExpr (threadIDColumnName :| []))
    voteCountSelectList =
      selectDerivedColumns
        [ deriveColumn $ columnReference (fieldColumnName Nothing threadIDField),
          deriveColumnAsAlias upvoteCountExpr (stringToAliasExpr "upvote_count"),
          deriveColumnAsAlias downVoteCountExpr (stringToAliasExpr "downvote_count")
        ]
    upvoteCountExpr =
      count
        ( caseExpr
            ( whenExpr
                (voteField `fieldEquals` True)
                (valueExpression (SqlValue.fromInt 1))
                :| []
            )
            (Just (valueExpression SqlValue.sqlNull))
        )
    downVoteCountExpr =
      count
        ( caseExpr
            ( whenExpr
                (voteField `fieldEquals` False)
                (valueExpression (SqlValue.fromInt 1))
                :| []
            )
            (Just (valueExpression SqlValue.sqlNull))
        )
    threadIDConstraint =
      columnReference (fieldColumnName (Just (stringToAliasName "s")) threadIDField)
        `equals` columnReference (fieldColumnName (Just (stringToAliasName "t")) threadIDField)
    commentCountSelectList =
      selectDerivedColumns
        [ deriveColumn $ columnReference (fieldColumnName Nothing threadIDField),
          deriveColumnAsAlias commentCountFieldExpr (stringToAliasExpr "comment_count")
        ]
    commentCountTable =
      subQueryAsFromItemExpr (stringToAliasExpr "comm") commentCountExpr
    commentCountFieldExpr =
      countColumn (fieldColumnName Nothing threadIDField)
    commentCountExpr =
      queryExpr selectClauseDefault commentCountSelectList (Just fromCommentTable)
    fromCommentTable =
      tableExpr
        (tableFromItem (tableName commentTable))
        Nothing
        (Just groupByThreadID)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
    joinList =
      [ joinExpr innerJoinType userTableName (joinOnConstraint userIDConstraint),
        joinExpr innerJoinType communityTableName (joinOnConstraint communityIDConstraint),
        joinExpr leftJoinType voteCountTable (joinOnConstraint threadIDConstraint),
        joinExpr leftJoinType commentCountTable (joinOnConstraint commentConstraint)
      ]
    fromTable wClause lClause oClause =
      mkTableExpr
        (threadTableName `appendJoinFromItem` joinList)
        defaultClauses
          { _whereClause = wClause,
          _limitExpr = lClause,
          _offSetExpr = oClause
          }
