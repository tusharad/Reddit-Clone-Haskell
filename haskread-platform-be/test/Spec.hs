{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Network.HTTP.Types
import qualified Orville.PostgreSQL as O
import Servant
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Wai
import TestAppConfig
import Servant.Auth.Server
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent
import Control.Monad.IO.Class

tests :: Application -> O.ConnectionPool -> BSL.ByteString -> TestTree
tests app pool token =
  testGroup
    "Tests"
    [ unitTests pool,
      apiTests app token
    ]

unitTests :: O.ConnectionPool -> TestTree
unitTests pool =
  testGroup
    "unitTests"
    [ testCase "2+2" $
        [1, 2, 3] `compare` [1, 2] @?= GT
    ]

apiTests :: Application -> BSL.ByteString -> TestTree
apiTests app token =
  testGroup
    "Servant HaskRead"
    [ testWai app "GET /check-health - 200" $ do
        res <- get "/check-health"
        assertStatus' status200 res,
      testWai app "Post /register user - 200" $ do
        res <-
          postWithHeaders
            "/api/v1/user/auth/register"
            registerUserBody
            [("Content-Type", "application/json")]
        assertStatus' status200 res,
      testWai app "Post /login user - " $ do
        res <-
          postWithHeaders
            "/api/v1/user/auth/login"
            incorrectLoginUserBody
            [("Content-Type", "application/json")]
        assertStatus' status400 res
        -- test case behaving abnormally, due to race condition
        -- There should be some sample data in the DB to test this
        -- testUserDashboard app token
    ]

testUserDashboard :: Application -> BSL.ByteString -> TestTree
testUserDashboard app token = 
  testWai app "get user dashboard" $ do

    liftIO $ threadDelay 1000
    res <- 
      srequest $ buildRequestWithHeaders GET "/api/v1/user/profile" "" [
        ("Content-Type", "application/json"),
        (hAuthorization, "Bearer " <> BSL.toStrict token)
      ]
    assertStatus' status200 res


main :: IO ()
main = do
  (myApp, pool,jwtSett) <- getTestAppCfg
  eToken <- makeJWT sampleUserInfo jwtSett Nothing
  case eToken of
    Left _ -> throwIO $ ErrorCall "JWT token creation failed"
    Right token -> do
      createDB pool
      defaultMain (tests myApp pool token)
        `catch` ( \(e :: SomeException) -> do
                    destroyDB pool
                    throwIO e
                )
