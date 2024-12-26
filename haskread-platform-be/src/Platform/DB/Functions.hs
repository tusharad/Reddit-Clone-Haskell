module Platform.DB.Functions
  ( updateUpdatedAtColumnFunctionQ,
    setUpdatedAtUsersTrigger,
  )
where

-- Module to define function/triggers for database.

import Data.List.NonEmpty
import Orville.PostgreSQL
import Orville.PostgreSQL.Expr
  ( orReplace,
    triggerBefore,
    triggerForEachRow,
    triggerOnUpdate,
  )
import Platform.DB.Table

{-
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
  NEW."updated_at" = NOW();
  RETURN NEW;
END;
\$$ LANGUAGE plpgsql;

create or replace TRIGGER set_updated_at_users
BEFORE UPDATE ON users
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();
-}

updateUpdatedAtColumnFunctionQ ::
  (MonadOrville m) =>
  m ()
updateUpdatedAtColumnFunctionQ =
  executeVoid OtherQuery $
    mkCreateFunctionExpr updateUpdatedAtTriggerFunctionDef (Just orReplace)

updateUpdatedAtTriggerFunctionDef :: FunctionDefinition
updateUpdatedAtTriggerFunctionDef =
  mkTriggerFunction
    "update_updated_at_column"
    plpgsql
    "BEGIN NEW.updated_at = NOW(); RETURN NEW; END;"

setUpdatedAtUsersTrigger ::
  (MonadOrville m) => m ()
setUpdatedAtUsersTrigger =
  mapM_
    runSetUpdatedAtTrigger
    [ (tableName userTable, "set_updated_at_users"),
      (tableName userProfileImageTable, "set_updated_at_userProfileImage"),
      (tableName adminTable, "set_updated_at_admin"),
      (tableName communityTable, "set_updated_at_community"),
      (tableName threadTable, "set_updated_at_thread"),
      (tableName threadVoteTable, "set_updated_at_threadVote"),
      (tableName commentTable, "set_updated_at_comment"),
      (tableName commentVoteTable, "set_updated_at_commentVote")
    ]
  where
    runSetUpdatedAtTrigger (tName, triggerName) =
      executeVoid OtherQuery $
        mkCreateTriggerExpr
          (setUpdatedAtTriggerDef triggerName)
          (Just orReplace)
          tName

setUpdatedAtTriggerDef :: String -> TriggerDefinition
setUpdatedAtTriggerDef triggerName =
  mkTriggerDefinition
    triggerName
    triggerBefore
    (triggerOnUpdate :| [])
    triggerForEachRow
    (functionName updateUpdatedAtTriggerFunctionDef)
