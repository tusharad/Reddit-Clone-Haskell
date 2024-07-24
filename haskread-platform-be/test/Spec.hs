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

tests :: Application -> O.ConnectionPool -> TestTree
tests app pool =
  testGroup
    "Tests"
    [ unitTests pool,
      apiTests app
    ]

unitTests :: O.ConnectionPool -> TestTree
unitTests pool =
  testGroup
    "unitTests"
    [ testCase "2+2" $
        [1, 2, 3] `compare` [1, 2] @?= GT
    ]

apiTests :: Application -> TestTree
apiTests app =
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
    ]

main :: IO ()
main = do
  (myApp, pool) <- getTestAppCfg
  createDB pool
  defaultMain (tests myApp pool)
    `catch` ( \(e :: SomeException) -> do
                destroyDB pool
                throwIO e
            )
