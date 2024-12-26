{-# LANGUAGE RecordWildCards #-}

module Platform.Orville.Helper
  ( selectClauseDefault
  , selectClauseDistinct
  , mkTableExpr
  , defaultClauses
  , fieldToAliasQualifiedColumnName
  , toTSVector
  , tsMatch
  , toTSQuery
  , plainToTSQuery
  , setWeight
  , stringConcat
  , tsRank
  , toWeight 
  , Regconfig (..)
  , Clauses (..)
  , Weight (..)
  )
where

import Data.Char (toLower)
import Orville.PostgreSQL.Expr hiding (tableName)
import Orville.PostgreSQL.Marshall (AliasName, FieldDefinition, fieldAliasQualifiedColumnName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

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

data Regconfig
  = Simple
  | Arabic
  | Armenian
  | Basque
  | Catalan
  | Danish
  | Dutch
  | English
  | Finnish
  | French
  | German
  | Greek
  | Hindi
  | Hungarian
  | Indonesian
  | Irish
  | Italian
  | Lithuanian
  | Nepali
  | Norwegian
  | Portuguese
  | Romanian
  | Russian
  | Serbian
  | Spanish
  | Swedish
  | Tamil
  | Turkish
  | Yiddish
  deriving (Eq, Show)

data Weight
  = A
  | B
  | C
  | D
  deriving (Eq, Show)

toWeight :: Weight -> ValueExpression
toWeight weight =
  RawSql.unsafeFromRawSql
    ( RawSql.fromString "\'"
        <> RawSql.fromString (map toLower (show weight))
        <> RawSql.fromString "\'"
    )

tsMatchOp :: BinaryOperator
tsMatchOp = binaryOperator "@@"

tsMatch :: ValueExpression -> ValueExpression -> BooleanExpr
tsMatch = binaryOpExpression tsMatchOp

stringConcatOp :: BinaryOperator
stringConcatOp = binaryOperator "||"

stringConcat :: ValueExpression -> ValueExpression -> ValueExpression
stringConcat = binaryOpExpression stringConcatOp

toTSVectorFunction :: FunctionName
toTSVectorFunction = functionName "to_tsvector"

toTSQueryFunction :: FunctionName
toTSQueryFunction = functionName "to_tsquery"

plainToTSQueryFunction :: FunctionName
plainToTSQueryFunction = functionName "plainto_tsquery"

tsRankFunction :: FunctionName
tsRankFunction = functionName "ts_rank"

setweightFunction :: FunctionName
setweightFunction = functionName "setweight"

regconfigToValueExpression :: Regconfig -> ValueExpression
regconfigToValueExpression regconfig =
  RawSql.unsafeFromRawSql
    ( RawSql.fromString "\'"
        <> RawSql.fromString (map toLower (show regconfig))
        <> RawSql.fromString "\'"
    )

toTSVector :: ValueExpression -> Maybe Regconfig -> ValueExpression
toTSVector val mRegconfig =
  let valueExpressions = case mRegconfig of
        Nothing -> mempty
        Just regconfig -> [regconfigToValueExpression regconfig]
   in functionCall toTSVectorFunction (valueExpressions <> [val])

toTSQuery :: ValueExpression -> Maybe Regconfig -> ValueExpression
toTSQuery val mRegconfig =
  let valueExpressions = case mRegconfig of
        Nothing -> mempty
        Just regconfig -> [regconfigToValueExpression regconfig]
   in functionCall toTSQueryFunction (valueExpressions <> [val])

plainToTSQuery :: ValueExpression -> Maybe Regconfig -> ValueExpression
plainToTSQuery val mRegconfig =
  let valueExpressions = case mRegconfig of
        Nothing -> mempty
        Just regconfig -> [regconfigToValueExpression regconfig]
   in functionCall plainToTSQueryFunction (valueExpressions <> [val])

setWeight :: ValueExpression -> Weight -> ValueExpression
setWeight val weight = functionCall setweightFunction [val, toWeight weight]

tsRank :: ValueExpression -> ValueExpression -> ValueExpression
tsRank tsVector tsQuery = functionCall tsRankFunction [tsVector, tsQuery]
