{-# LANGUAGE RecordWildCards #-}

module Platform.Orville.Helper
  ( selectClauseDefault
  , selectClauseDistinct
  , mkTableExpr
  , defaultClauses
  , fieldToAliasQualifiedColumnName
  , Clauses (..)
  )
where

import Orville.PostgreSQL.Expr hiding (tableName)
import Orville.PostgreSQL.Marshall (AliasName, FieldDefinition, fieldAliasQualifiedColumnName)

selectClauseDefault :: SelectClause
selectClauseDefault = selectClause (selectExpr Nothing)

selectClauseDistinct :: SelectClause
selectClauseDistinct = selectClause (selectExpr $ Just Distinct)

data Clauses = Clauses
  { _whereClause :: Maybe WhereClause
  , _groupByClause :: Maybe GroupByClause
  , _orderByClause :: Maybe OrderByClause
  , _limitExpr :: Maybe LimitExpr
  , _offSetExpr :: Maybe OffsetExpr
  , _rowLockingClause :: Maybe RowLockingClause
  , _windowClause :: Maybe WindowClause
  , _fetchClause :: Maybe FetchClause
  }

defaultClauses :: Clauses
defaultClauses =
  Clauses
    { _whereClause = Nothing
    , _groupByClause = Nothing
    , _orderByClause = Nothing
    , _limitExpr = Nothing
    , _offSetExpr = Nothing
    , _rowLockingClause = Nothing
    , _windowClause = Nothing
    , _fetchClause = Nothing
    }

mkTableExpr :: FromItemExpr -> Clauses -> TableExpr
mkTableExpr f Clauses {..} =
  tableExpr
    f
    _whereClause
    _groupByClause
    _orderByClause
    _limitExpr
    _offSetExpr
    _rowLockingClause
    _windowClause
    _fetchClause

fieldToAliasQualifiedColumnName ::
  AliasName -> FieldDefinition nullability a -> QualifiedOrUnqualified ColumnName
fieldToAliasQualifiedColumnName x y = untrackQualified $ fieldAliasQualifiedColumnName x y
