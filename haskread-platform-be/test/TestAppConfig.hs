{-# LANGUAGE OverloadedStrings #-}

module TestAppConfig where

import Orville.PostgreSQL
import qualified Orville.PostgreSQL as O
import Orville.PostgreSQL.AutoMigration
import Platform.Admin.Community.DB
import Platform.Admin.DB
import Platform.Comment.DB
import Platform.Common.Types
import Platform.Common.Utils
import Platform.Core (app)
import Platform.DB.Functions
import Platform.DB.Model
import Platform.DB.Table
import Platform.User.DB
import Platform.User.Thread.DB
import Servant
import Servant.Auth.Server
import System.Exit
import System.Log.FastLogger
import TestApp.SampleData

getTestAppCfg :: IO (Application, ConnectionPool, JWTSettings)
getTestAppCfg = do
  eEnv <- readEnv "./testEnv.dhall"
  case eEnv of
    Left e -> (putStrLn $ show e) >> exitFailure
    Right (appST, jwtSett, ctx, _, pool) -> do
      return (app appST jwtSett ctx, pool, jwtSett)

schemaList :: [SchemaItem]
schemaList =
  [ SchemaTable userTable,
    SchemaTable adminTable,
    SchemaTable userProfileImageTable,
    SchemaTable communityTable,
    SchemaTable threadTable,
    SchemaTable threadVoteTable,
    SchemaTable commentTable,
    SchemaTable commentVoteTable
  ]

schemaDropList :: [SchemaItem]
schemaDropList =
  [ SchemaDropTable $ tableIdentifier userProfileImageTable,
    SchemaDropTable $ tableIdentifier threadVoteTable,
    SchemaDropTable $ tableIdentifier commentVoteTable,
    SchemaDropTable $ tableIdentifier commentTable,
    SchemaDropTable $ tableIdentifier threadTable,
    SchemaDropTable $ tableIdentifier communityTable,
    SchemaDropTable $ tableIdentifier userTable,
    SchemaDropTable $ tableIdentifier adminTable
  ]

insertUserSampleData :: (MonadOrville m) => [UserWrite] -> m ()
insertUserSampleData = mapM_ addUserQ

insertAdminSampleData :: (MonadOrville m) => [AdminWrite] -> m ()
insertAdminSampleData = mapM_ addAdminQ

insertCommunitySampleData :: (MonadOrville m) => [CommunityWrite] -> m ()
insertCommunitySampleData = mapM_ addCommunityQ

insertThreadSampleData :: (MonadOrville m) => [ThreadWrite] -> m ()
insertThreadSampleData = mapM_ addThreadQ

insertCommentSampleData :: (MonadOrville m) => [CommentWrite] -> m ()
insertCommentSampleData = mapM_ addCommentQ

createDB :: ConnectionPool -> IO ()
createDB pool = runOrville pool $ do
  autoMigrateSchema defaultOptions schemaList
  updateUpdatedAtColumnFunctionQ
  setUpdatedAtUsersTrigger
  insertUserSampleData sampleUsers
  insertAdminSampleData sampleAdmins
  insertCommunitySampleData sampleCommunities
  insertThreadSampleData sampleThreads
  insertCommentSampleData sampleComments

destroyDB :: ConnectionPool -> IO ()
destroyDB pool = runOrville pool $ autoMigrateSchema defaultOptions schemaDropList
