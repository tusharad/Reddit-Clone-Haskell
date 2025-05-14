{-# LANGUAGE OverloadedStrings #-}

module Platform.Common.DB (autoMigrateQ, checkDBConnection) where

import Orville.PostgreSQL
import Orville.PostgreSQL.AutoMigration
import Platform.DB.Table
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import Control.Exception (try, SomeException)
import Control.Monad (void)

schemaList :: [SchemaItem]
schemaList =
  [ SchemaTable userTable,
    SchemaTable adminTable,
    SchemaTable userProfileImageTable,
    SchemaTable communityTable,
    SchemaTable threadTable,
    SchemaTable threadVoteTable,
    SchemaTable commentTable,
    SchemaTable commentVoteTable,
    SchemaTable userEmailVerifyOTPTable
  ]

autoMigrateQ :: MonadOrville m => m ()
autoMigrateQ = autoMigrateSchema defaultOptions schemaList

checkDBConnection :: ConnectionPool -> IO (Either SomeException ())
checkDBConnection pool = do 
  try $ Conn.withPoolConnection pool $ \conn -> 
    void $ Conn.executeRaw conn "select 1+1;" []
