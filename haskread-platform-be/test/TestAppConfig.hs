{-# LANGUAGE OverloadedStrings #-}

module TestAppConfig
  ( getTestAppCfg,
    createDB,
    destroyDB,
    registerUserBody,
    incorrectLoginUserBody,
    sampleUserInfo
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
import Platform.User.Types hiding (RegisterUserBody (..))
import qualified Platform.User.Types as UT (RegisterUserBody (..))
import Servant
import Servant.Auth.Server
import Test.Tasty
import Platform.Auth.Types
import Platform.DB.Model (UserID(..))

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

setDefaultNow colName = alterColumnSetDefault (columnName colName) now

mkTableFieldDefault tableDef colName =
  alterTableExpr
    (O.tableName tableDef)
    (setDefaultNow colName :| [])

createDB :: ConnectionPool -> IO ()
createDB pool = do
  runOrville pool $ do
    autoMigrateSchema defaultOptions schemaList
    executeVoid DDLQuery $ mkTableFieldDefault userTable "created_at"
    executeVoid DDLQuery $ mkTableFieldDefault userTable "updated_at"
  print "database created"

destroyDB :: ConnectionPool -> IO ()
destroyDB pool = do
  print "inside truncate DB"
  runOrville pool $ autoMigrateSchema defaultOptions schemaDropList
  print "database deleted"

registerUserBody :: ByteString
registerUserBody =
  let r =
        UT.RegisterUserBody
          { UT.userName = "tushar",
            UT.email = "tushar@abc",
            UT.password = "Tushar123",
            UT.confirmPassword = "Tushar123"
          }
   in encode r

incorrectLoginUserBody :: ByteString
incorrectLoginUserBody =
  let l =
        LoginUserBody
          { email = "tushar@abc.com",
            password = "1234"
          }
   in encode l

sampleUserInfo :: UserInfo
sampleUserInfo = UserInfo {
    userIDForUserInfo = UserID 1
 ,  userNameForUserInfo = "asd"
}