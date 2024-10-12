module Platform.Comment.DB
  ( addCommentQ,
    fetchCommentByIDQ,
    deleteCommentQ,
    updateCommentQ,
    fetchCommentVoteQ,
    addCommentVoteQ,
    deleteCommentVoteQ,
    updateCommentVoteQ,
    fetchCommentsByThreadQ,
  )
where

import Orville.PostgreSQL
import Orville.PostgreSQL.Expr hiding (tableName)
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import Platform.Orville.Helper

addCommentQ :: (MonadOrville m) => CommentWrite -> m ()
addCommentQ = insertEntity commentTable

fetchCommentByIDQ :: (MonadOrville m) => CommentID -> m (Maybe CommentRead)
fetchCommentByIDQ = findEntity commentTable

deleteCommentQ :: (MonadOrville m) => CommentID -> m ()
deleteCommentQ = deleteEntity commentTable

updateCommentQ :: (MonadOrville m) => CommentID -> CommentWrite -> m ()
updateCommentQ = updateEntity commentTable

fetchCommentVoteQ :: (MonadOrville m) => CommentID -> UserID -> m (Maybe CommentVoteRead)
fetchCommentVoteQ cID uID = findEntity commentVoteTable (CommentVoteID uID cID)

addCommentVoteQ :: (MonadOrville m) => CommentVoteWrite -> m ()
addCommentVoteQ = insertEntity commentVoteTable

deleteCommentVoteQ :: (MonadOrville m) => CommentID -> UserID -> m ()
deleteCommentVoteQ cID uID = deleteEntity commentVoteTable (CommentVoteID uID cID)

updateCommentVoteQ :: (MonadOrville m) => CommentID -> UserID -> CommentVoteWrite -> m ()
updateCommentVoteQ cID uID = updateEntity commentVoteTable (CommentVoteID uID cID)

fetchCommentsByThreadQ :: (MonadOrville m) => ThreadID -> m [CommentInfo]
fetchCommentsByThreadQ threadID = do
  executeAndDecode
    SelectQuery
    (fetchCommentsByThreadExpr threadID)
    (annotateSqlMarshallerEmptyAnnotation commentInfoMarshaller)

whereThreadIdIs :: ThreadID -> WhereClause
whereThreadIdIs (ThreadID tID) =
  whereClause $
    columnReference (fieldColumnName (Just (stringToAliasName "c")) threadIDField)
      `equals` valueExpression (SqlValue.fromInt32 tID)

fetchCommentsByThreadExpr :: ThreadID -> QueryExpr
fetchCommentsByThreadExpr threadID =
  queryExpr selectClause_ selectedColumns (Just fromTable)
  where
    selectClause_ = selectClause (selectExpr Nothing)
    selectedColumns =
      selectColumns
        [ fieldColumnName (Just (stringToAliasName "c")) commentIDField,
          fieldColumnName (Just (stringToAliasName "c")) commentContentField,
          fieldColumnName (Just (stringToAliasName "c")) userIDField,
          fieldColumnName (Just (stringToAliasName "u")) userNameField,
          fieldColumnName (Just (stringToAliasName "c")) threadIDField,
          fieldColumnName (Just (stringToAliasName "c")) createdAtField,
          fieldColumnName (Just (stringToAliasName "c")) parentCommentIDField
        ]
    commentTableName = tableFromItemWithAlias (stringToAliasExpr "c") (tableName commentTable)
    userTableName = tableFromItemWithAlias (stringToAliasExpr "u") (tableName userTable)
    userIDConstraint =
      columnReference (fieldColumnName (Just (stringToAliasName "u")) userIDField)
        `equals` columnReference (fieldColumnName (Just (stringToAliasName "c")) userIDField)
    joinList =
      [joinExpr innerJoinType userTableName (joinOnConstraint userIDConstraint)]
    fromTable =
      mkTableExpr
        (commentTableName `appendJoinFromItem` joinList)
        defaultClauses { _whereClause = Just $ whereThreadIdIs threadID}
