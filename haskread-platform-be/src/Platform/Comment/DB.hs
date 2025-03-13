module Platform.Comment.DB
  ( addCommentQ
  , fetchCommentByIDQ
  , deleteCommentQ
  , updateCommentQ
  , fetchCommentVoteQ
  , addCommentVoteQ
  , deleteCommentVoteQ
  , updateCommentVoteQ
  , fetchCommentsByThreadQ
  , fetchVoteCommentsByUser
  )
where

import Data.Coerce (coerce)
import Data.List.NonEmpty
import Orville.PostgreSQL
import Orville.PostgreSQL.Expr hiding (tableName)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import Platform.Common.AppM (AppM)
import Platform.Common.Utils (queryWrapper)
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import Platform.Orville.Helper
import UnliftIO (MonadUnliftIO)

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
fetchCommentsByThreadQ _threadID = do
  executeAndDecode
    SelectQuery
    (fetchCommentsByThreadExpr _threadID)
    (annotateSqlMarshallerEmptyAnnotation commentInfoMarshaller)

whereThreadIdIs :: ThreadID -> WhereClause
whereThreadIdIs (ThreadID tID) =
  whereClause $
    columnReference (fieldToAliasQualifiedColumnName ((stringToAliasName "c")) threadIDField)
      `equals` valueExpression (SqlValue.fromInt32 tID)

fetchCommentsByThreadExpr :: ThreadID -> QueryExpr
fetchCommentsByThreadExpr _threadID =
  queryExpr selectClause_ selectedColumns (Just fromTable)
  where
    selectClause_ = selectClause (selectExpr Nothing)
    selectedColumns =
      selectColumns
        [ fieldToAliasQualifiedColumnName (stringToAliasName "c") commentIDField
        , fieldToAliasQualifiedColumnName (stringToAliasName "c") commentContentField
        , fieldToAliasQualifiedColumnName (stringToAliasName "c") userIDField
        , fieldToAliasQualifiedColumnName (stringToAliasName "u") userNameField
        , fieldToAliasQualifiedColumnName (stringToAliasName "c") threadIDField
        , fieldToAliasQualifiedColumnName (stringToAliasName "c") createdAtField
        , fieldToAliasQualifiedColumnName (stringToAliasName "c") parentCommentIDField
        , fieldColumnName upvoteCountField
        , fieldColumnName downvoteCountField
        ]
    commentTableName = tableFromItemWithAlias (stringToAliasExpr "c") (tableName commentTable)
    userTableName = tableFromItemWithAlias (stringToAliasExpr "u") (tableName userTable)
    userIDConstraint =
      columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "u") userIDField)
        `equals` columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "c") userIDField)
    commentIdConstraint =
      columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "c") commentIDField)
        `equals` columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "v") commentIDField)
    voteCountTable =
      subQueryAsFromItemExpr (stringToAliasExpr "v") voteCountExpr
    voteCountExpr =
      queryExpr selectClauseDefault voteCountSelectList (Just fromVoteCommentTable)
    groupByCommentID =
      groupByClause (groupByColumnsExpr (fieldColumnName commentIDField :| []))
    voteCountSelectList =
      selectDerivedColumns
        [ deriveColumn $ columnReference (fieldColumnName commentIDField)
        , deriveColumnAsAlias upvoteCountExpr (stringToAliasExpr "upvote_count")
        , deriveColumnAsAlias downVoteCountExpr (stringToAliasExpr "downvote_count")
        ]
    upvoteCountExpr =
      countExprAggregateFunction
        ( caseExpr
            ( whenExpr
                (voteField `fieldEquals` True)
                (valueExpression (SqlValue.fromInt 1))
                :| []
            )
            (Just (valueExpression SqlValue.sqlNull))
        )
    downVoteCountExpr =
      countExprAggregateFunction
        ( caseExpr
            ( whenExpr
                (voteField `fieldEquals` False)
                (valueExpression (SqlValue.fromInt 1))
                :| []
            )
            (Just (valueExpression SqlValue.sqlNull))
        )
    fromVoteCommentTable =
      tableExpr
        (tableFromItem (tableName commentVoteTable))
        Nothing
        (Just groupByCommentID)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
    joinList =
      [ joinExpr innerJoinType userTableName (joinOnConstraint userIDConstraint)
      , joinExpr leftJoinType voteCountTable (joinOnConstraint commentIdConstraint)
      ]
    fromTable =
      mkTableExpr
        (commentTableName `appendJoinFromItem` joinList)
        defaultClauses {_whereClause = Just $ whereThreadIdIs _threadID}

fetchVoteCommentsByUser ::
  MonadUnliftIO m =>
  UserID ->
  [CommentID] ->
  AppM m [CommentVoteRead]
fetchVoteCommentsByUser _userID commentIdList =
  queryWrapper $
    findEntitiesBy
      commentVoteTable
      ( where_
          ( fieldColumnReference userIDField
              `equals` valueExpression
                (SqlValue.fromInt32 $ coerce _userID)
              .&& valueIn
                (fieldColumnReference commentIDField)
                (fromList (valueExpression . SqlValue.fromInt32 . coerce <$> commentIdList))
          )
      )
