{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isRight)
import Network.HTTP.Types
import qualified Orville.PostgreSQL as O
import Servant
import Servant.Auth.Server
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Wai
import TestAppConfig

tests :: Application -> O.ConnectionPool -> [BSL.ByteString] -> TestTree
tests app pool tokens =
  testGroup
    "Tests"
    [ unitTests pool,
      apiTests app tokens
    ]

unitTests :: O.ConnectionPool -> TestTree
unitTests _ =
  testGroup
    "unitTests"
    [ testCase "2+2" $
        ([1, 2, 3] :: [Int]) `compare` [1, 2] @?= GT -- Infering to avoid warning
    ]

apiTests :: Application -> [BSL.ByteString] -> TestTree
apiTests app [token1, token2, token3] =
  testGroup
    "Servant HaskRead"
    [ testWai app "GET /check-health - 200" $ do
        res <- get "/check-health"
        assertStatus' status200 res,
      testWai app "POST /register user - 200" $ do
        res <-
          postWithHeaders
            "/api/v1/user/auth/register"
            registerUserBody
            [("Content-Type", "application/json")]
        assertStatus' status200 res,
      testWai app "POST /login user - 200" $ do
        res <-
          postWithHeaders
            "/api/v1/user/auth/login"
            correctLoginUserBody
            [("Content-Type", "application/json")]
        assertStatus' status200 res,
      testUserDashboard app token2,
      testUserChangePassword app token1,
      testUserDeleteAccount app token3
    ]
apiTests _ _ = testGroup "Servant HaskRead" []

testUserDeleteAccount :: Application -> BSL.ByteString -> TestTree
testUserDeleteAccount app token =
  testWai app "DELETE /user/profile/delete-account - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          DELETE
          "/api/v1/user/profile/delete-account"
          sampleDeleteUserBody
          [ ("Content-Type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ]
    assertStatus' status200 res

testUserChangePassword :: Application -> BSL.ByteString -> TestTree
testUserChangePassword app token =
  testWai app "PUT /user/profile/change-password - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          PUT
          "/api/v1/user/profile/change-password"
          sampleChangePasswordBody
          [ ("Content-Type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ]
    assertStatus' status200 res

testUserDashboard :: Application -> BSL.ByteString -> TestTree
testUserDashboard app token =
  testWai app "GET /user/profile - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          GET
          "/api/v1/user/profile"
          ""
          [ ("Content-Type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ]
    assertStatus' status200 res

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z = f z y x

main :: IO ()
main = do
  (myApp, pool, jwtSett) <- getTestAppCfg
  eTokens <- mapM (flip3 makeJWT Nothing jwtSett) sampleUserInfoList
  ( if all isRight eTokens
      then
        ( do
            let tokens = fmap (\(Right t) -> t) eTokens
            createDB pool
            defaultMain (tests myApp pool tokens)
              `catch` ( \(e :: SomeException) -> do
                          destroyDB pool
                          throwIO e
                      )
        )
      else throwIO $ ErrorCall "JWT token creation failed"
    )
