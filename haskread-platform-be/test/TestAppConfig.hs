{-# LANGUAGE OverloadedStrings #-}

module TestAppConfig
  ( getTestAppCfg,
    createDB,
    destroyDB,
    registerUserBody,
    correctLoginUserBody,
    sampleUserInfoList,
    sampleChangePasswordBody,
    sampleDeleteUserBody,
    sampleUsers
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty
import Orville.PostgreSQL
import qualified Orville.PostgreSQL as O
import Orville.PostgreSQL.AutoMigration
import Orville.PostgreSQL.Expr
import Platform.Common.Types
import Platform.Core (app)
import Platform.DB.Table (userTable)
import Platform.User.Types
import Servant
import Servant.Auth.Server
import Platform.Auth.Types
import Platform.DB.Model
import Platform.User.DB

connectionOptionsForTest :: ConnectionOptions
connectionOptionsForTest =
  ConnectionOptions
    { connectionString =
        "dbname=haskread_test_db host=localhost user=tushar password=1234",
      connectionNoticeReporting = DisableNoticeReporting,
      connectionPoolStripes = OneStripePerCapability,
      connectionPoolMaxConnections = MaxConnectionsPerStripe 1,
      connectionPoolLingerTime = 10
    }

getTestAppCfg :: IO (Application, ConnectionPool, JWTSettings)
getTestAppCfg = do
  pool <- createConnectionPool connectionOptionsForTest
  jwtSecretKey <- generateKey
  let orvilleState = O.newOrvilleState O.defaultErrorDetailLevel pool
      appST = MyAppState (AppConfig "uploads") orvilleState
      jwtSett = defaultJWTSettings jwtSecretKey
  let ctx = defaultCookieSettings :. jwtSett :. EmptyContext
  return (app appST jwtSett ctx, pool,jwtSett)

schemaList :: [SchemaItem]
schemaList =
  [ SchemaTable userTable
  ]

schemaDropList :: [SchemaItem]
schemaDropList =
  [ SchemaDropTable $ tableIdentifier userTable
  ]

setDefaultNow :: String -> AlterTableAction
setDefaultNow colName = alterColumnSetDefault (columnName colName) now

mkTableFieldDefault :: TableDefinition key writeEntity readEntity -> String -> AlterTableExpr
mkTableFieldDefault tableDef colName =
  alterTableExpr
    (O.tableName tableDef)
    (setDefaultNow colName :| [])

sampleUsers :: [UserWrite]
sampleUsers = [
  User {
    userName = "batman",
    email = "bruce@abc.com",
    password = "Bruce123",
    userID = (),
    createdAt = (),
    updatedAt = ()
  },
  User {
    userName = "spiderman",
    email = "peter@abc.com",
    password = "Peter123",
    userID = (),
    createdAt = (),
    updatedAt = ()
  },
  User {
    userName = "superman",
    email = "clark@abc.com",
    password = "Clark123",
    userID = (),
    createdAt = (),
    updatedAt = ()
  } ]

sampleUserInfoList :: [UserInfo]
sampleUserInfoList = [
  UserInfo {
    userIDForUserInfo = UserID 1,
    userNameForUserInfo = "batman"
  }
  , UserInfo {
    userIDForUserInfo = UserID 2,
    userNameForUserInfo = "spiderman"
  }
  , UserInfo {
    userIDForUserInfo = UserID 3,
    userNameForUserInfo = "superman"
  }]

insertUserSampleData :: MonadOrville m => [UserWrite] -> m ()
insertUserSampleData = mapM_ addUserQ
  
createDB :: ConnectionPool -> IO ()
createDB pool = do
  runOrville pool $ do
    autoMigrateSchema defaultOptions schemaList
    executeVoid DDLQuery $ mkTableFieldDefault userTable "created_at"
    executeVoid DDLQuery $ mkTableFieldDefault userTable "updated_at"
    insertUserSampleData sampleUsers

destroyDB :: ConnectionPool -> IO ()
destroyDB pool = do
  runOrville pool $ autoMigrateSchema defaultOptions schemaDropList

registerUserBody :: ByteString
registerUserBody =
  let r =
        RegisterUserBody
          { userNameForRegister = "tushar",
            emailForRegister = "tushar@abc",
            passwordForRegister = "Tushar123",
            confirmPasswordForRegister = "Tushar123"
          }
   in encode r

correctLoginUserBody :: ByteString
correctLoginUserBody =
  let l =
        LoginUserBody
          { emailForLogin = "peter@abc.com",
            passwordForLogin = "Peter123"
          }
   in encode l

sampleChangePasswordBody :: ByteString
sampleChangePasswordBody = encode ChangePasswordBody {
  oldPasswordForChangePass = "Bruce123"
, newPasswordForChangePass = "Bruce1235"
, confirmPasswordForChangePass = "Bruce1235"
}

sampleDeleteUserBody :: ByteString
sampleDeleteUserBody = encode DeleteUserBody {
  passwordForDeleteUser = "Clark123",
  areUSure = True
}