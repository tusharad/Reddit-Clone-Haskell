{-# LANGUAGE OverloadedStrings #-}

module Platform.User.Thread.DB
  ( fetchThreadByIDQ
  , addThreadQ
  , updateThreadQ
  , deleteThreadQ
  , fetchThreadInfoQ
  , fetchThreadInfoByIDQ
  , fetchThreadInfoByTextQ
  , fetchAllThreads
  )
where

-- import qualified Orville.PostgreSQL.Expr as Expr

import Data.List.NonEmpty
import Data.Text (Text)
import Orville.PostgreSQL
import Orville.PostgreSQL.Expr hiding (tableName)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table
import Platform.Orville.Helper

fetchAllThreads :: MonadOrville m => m [ThreadRead]
fetchAllThreads = findEntitiesBy threadTable emptySelectOptions

fetchThreadByIDQ :: (MonadOrville m) => ThreadID -> m (Maybe ThreadRead)
fetchThreadByIDQ = findEntity threadTable

addThreadQ :: (MonadOrville m) => ThreadWrite -> m ()
addThreadQ = insertEntity threadTable

updateThreadQ :: (MonadOrville m) => ThreadID -> ThreadWrite -> m ()
updateThreadQ = updateEntity threadTable

deleteThreadQ :: (MonadOrville m) => ThreadID -> m ()
deleteThreadQ = deleteEntity threadTable

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
    columnReference
      ( untrackQualified
          (fieldAliasQualifiedColumnName (stringToAliasName "t") threadIDField)
      )
      `equals` valueExpression (SqlValue.fromInt32 tID)

fetchThreadInfoByIDQ :: (MonadOrville m) => ThreadID -> m (Maybe ThreadInfo)
fetchThreadInfoByIDQ tID = do
  res <-
    executeAndDecode
      SelectQuery
      (fetchThreadInfoExpr (Just $ whereThreadIdIs tID) Nothing Nothing Nothing)
      (annotateSqlMarshallerEmptyAnnotation threadInfoMarshaller)
  case res of
    [] -> pure Nothing
    (x : _) -> pure $ Just x

mkWhereClauseForFetchThreadInfo :: Maybe Int -> Maybe Int -> Maybe WhereClause
mkWhereClauseForFetchThreadInfo Nothing Nothing = Nothing
mkWhereClauseForFetchThreadInfo (Just cId) Nothing = Just $ whereClause (communityIdExpr cId)
mkWhereClauseForFetchThreadInfo Nothing (Just uId) = Just $ whereClause (userIdExpr uId)
mkWhereClauseForFetchThreadInfo (Just cId) (Just uId) =
  Just $
    whereClause (userIdExpr uId .&& communityIdExpr cId)

communityIdExpr :: Int -> BooleanExpr
communityIdExpr cId =
  columnReference
    ( untrackQualified $
        fieldAliasQualifiedColumnName (stringToAliasName "t") communityIDField
    )
    `equals` valueExpression (SqlValue.fromInt32 $ fromIntegral cId)

userIdExpr :: Int -> BooleanExpr
userIdExpr uId =
  columnReference
    ( untrackQualified $
        fieldAliasQualifiedColumnName (stringToAliasName "t") userIDField
    )
    `equals` valueExpression (SqlValue.fromInt32 $ fromIntegral uId)

fetchThreadInfoQ :: (MonadOrville m) => Int -> Int -> Maybe Int -> Maybe Int -> m [ThreadInfo]
fetchThreadInfoQ _limit _offset mCommunityId mUserId =
  executeAndDecode
    SelectQuery
    ( fetchThreadInfoExpr
        (mkWhereClauseForFetchThreadInfo mCommunityId mUserId)
        (Just (limitExpr _limit))
        (Just (offsetExpr _offset))
        Nothing
    )
    (annotateSqlMarshallerEmptyAnnotation threadInfoMarshaller)

fetchThreadInfoExpr ::
  Maybe WhereClause ->
  Maybe LimitExpr ->
  Maybe OffsetExpr ->
  Maybe OrderByClause ->
  QueryExpr
fetchThreadInfoExpr wClause lClause oClause orderClause =
  queryExpr
    selectClauseDefault
    selectedColumns
    (Just (fromTable wClause lClause oClause orderClause))
  where
    selectedColumns =
      selectColumns
        [ fieldToAliasQualifiedColumnName (stringToAliasName "t") threadIDField
        , fieldToAliasQualifiedColumnName (stringToAliasName "t") threadTitleField
        , fieldToAliasQualifiedColumnName (stringToAliasName "t") threadDescriptionField
        , fieldToAliasQualifiedColumnName (stringToAliasName "t") createdAtField
        , fieldToAliasQualifiedColumnName (stringToAliasName "t") threadAttachmentField
        , fieldToAliasQualifiedColumnName (stringToAliasName "t") threadAttachmentNameField
        , fieldToAliasQualifiedColumnName (stringToAliasName "t") threadAttachmentSizeField
        , fieldToAliasQualifiedColumnName (stringToAliasName "u") userIDField
        , fieldToAliasQualifiedColumnName (stringToAliasName "u") userNameField
        , fieldToAliasQualifiedColumnName (stringToAliasName "c") communityIDField
        , fieldToAliasQualifiedColumnName (stringToAliasName "c") communityNameField
        , fieldColumnName upvoteCountField
        , fieldColumnName downvoteCountField
        , fieldColumnName commentCountField
        ]
    threadIDColumnName = fieldColumnName threadIDField
    threadTableName = tableFromItemWithAlias (stringToAliasExpr "t") (tableName threadTable)
    userTableName = tableFromItemWithAlias (stringToAliasExpr "u") (tableName userTable)
    communityTableName =
      tableFromItemWithAlias
        (stringToAliasExpr "c")
        (tableName communityTable)
    userIDConstraint =
      columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "u") userIDField)
        `equals` columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "t") userIDField)
    communityIDConstraint =
      columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "c") communityIDField)
        `equals` columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "t") communityIDField)
    commentConstraint =
      columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "comm") threadIDField)
        `equals` columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "t") threadIDField)
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
        Nothing
    groupByThreadID =
      groupByClause (groupByColumnsExpr (threadIDColumnName :| []))
    voteCountSelectList =
      selectDerivedColumns
        [ deriveColumn $ columnReference (fieldColumnName threadIDField)
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
    threadIDConstraint =
      columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "s") threadIDField)
        `equals` columnReference (fieldToAliasQualifiedColumnName (stringToAliasName "t") threadIDField)
    commentCountSelectList =
      selectDerivedColumns
        [ deriveColumn $ columnReference (fieldColumnName threadIDField)
        , deriveColumnAsAlias commentCountFieldExpr (stringToAliasExpr "comment_count")
        ]
    commentCountTable =
      subQueryAsFromItemExpr (stringToAliasExpr "comm") commentCountExpr
    commentCountFieldExpr =
      countColumnAggregateFunction (fieldColumnName threadIDField)
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
        Nothing
    joinList =
      [ joinExpr innerJoinType userTableName (joinOnConstraint userIDConstraint)
      , joinExpr innerJoinType communityTableName (joinOnConstraint communityIDConstraint)
      , joinExpr leftJoinType voteCountTable (joinOnConstraint threadIDConstraint)
      , joinExpr leftJoinType commentCountTable (joinOnConstraint commentConstraint)
      ]
    fromTable _wClause _lClause _oClause _orderClause =
      mkTableExpr
        (threadTableName `appendJoinFromItem` joinList)
        defaultClauses
          { _whereClause = _wClause
          , _limitExpr = _lClause
          , _offSetExpr = _oClause
          , _orderByClause = _orderClause
          }

{-
SELECT
    thread_title,
    thread_description
FROM thread
WHERE
    to_tsvector('english', thread_title) @@ plainto_tsquery('english', 'haskell')
    OR
    to_tsvector('english', thread_description) @@ plainto_tsquery('english', 'haskell')
ORDER BY ts_rank(
        setweight(to_tsvector('english', thread_title), 'A') ||
        setweight(to_tsvector('english', thread_description), 'B'),
        to_tsquery('english', 'is')
    ) DESC;
-}

mkWhereClauseForFetchThreadInfoByText :: Text -> Maybe WhereClause
mkWhereClauseForFetchThreadInfoByText txt =
  Just $
    whereClause $
      go threadTitleField
        .|| go threadDescriptionField
  where
    go t =
      tsMatch
        (toTSVector (columnToValExpression t) (Just englishRegConfig))
        (textToPlainTSQuery txt)

mkOrderByClauseForFetchThreadInfoByText :: Text -> Maybe OrderByClause
mkOrderByClauseForFetchThreadInfoByText txt =
  Just $
    orderByClause $
      orderByValueExpression
        ( toTSRank
            ( tsVectorConcat
                ( setTSWeight (toTSVector (columnToValExpression threadTitleField) (Just englishRegConfig)) tsWeightA
                )
                ( setTSWeight
                    (toTSVector (columnToValExpression threadDescriptionField) (Just englishRegConfig))
                    tsWeightB
                )
            )
            (textToTSQuery txt)
        )
        ascendingOrder

-- simple internal functions
columnToValExpression :: FieldDefinition nullability a -> ValueExpression
columnToValExpression t =
  columnReference (untrackQualified (fieldAliasQualifiedColumnName (stringToAliasName "t") t))

textToPlainTSQuery :: Text -> TSQuery
textToPlainTSQuery txt = plainToTSQuery (valueExpression $ SqlValue.fromText txt) (Just englishRegConfig)

textToTSQuery :: Text -> TSQuery
textToTSQuery txt = toTSQuery (valueExpression $ SqlValue.fromText txt) (Just englishRegConfig)

fetchThreadInfoByTextQ :: MonadOrville m => Text -> m [ThreadInfo]
fetchThreadInfoByTextQ searchTerm =
  executeAndDecode
    SelectQuery
    ( fetchThreadInfoExpr
        (mkWhereClauseForFetchThreadInfoByText searchTerm)
        (Just (limitExpr 10))
        Nothing
        (mkOrderByClauseForFetchThreadInfoByText searchTerm)
    )
    (annotateSqlMarshallerEmptyAnnotation threadInfoMarshaller)
