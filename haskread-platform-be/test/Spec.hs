{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isRight)
import Network.HTTP.Types
import qualified Orville.PostgreSQL as O
import Servant
import Servant.Auth.Server
import Test.Tasty
import Test.Tasty.Wai
import TestApp.Admin
import TestApp.Auth
import TestApp.Thread
import TestApp.Community
import TestApp.SampleData
import TestApp.Users
import TestAppConfig

tests ::
  Application ->
  O.ConnectionPool ->
  [BSL.ByteString] ->
  [BSL.ByteString] ->
  TestTree
tests app pool userTokens adminTokens =
  testGroup "Tests" [apiTests app userTokens adminTokens]

apiTests :: Application -> [BSL.ByteString] -> [BSL.ByteString] -> TestTree
apiTests app userTokens adminTokens =
  testGroup
    "Servant HaskRead"
    [ testCheckHealth app,
      authAPITests app,
      userAPITests app userTokens,
      adminAPITests app adminTokens,
      communityAPITests app adminTokens,
      threadAPITests app userTokens
    ]

testCheckHealth :: Application -> TestTree
testCheckHealth app =
  testWai app "GET /check-health - 200" $ do
    res <- get "/check-health"
    assertStatus' status200 res

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z = f z y x

main :: IO ()
main = do
  (myApp, pool, jwtSett) <- getTestAppCfg
  eUserTokens <- mapM (flip3 makeJWT Nothing jwtSett) sampleUserInfoList
  eAdminTokens <- mapM (flip3 makeJWT Nothing jwtSett) sampleAdminInfoList
  ( if all isRight $ eUserTokens <> eAdminTokens
      then
        ( do
            let userTokens = fmap (\(Right t) -> t) eUserTokens
                adminTokens = fmap (\(Right t) -> t) eAdminTokens
            createDB pool
            defaultMain (tests myApp pool userTokens adminTokens)
              `catch` ( \(e :: SomeException) -> do
                          destroyDB pool
                          throwIO e
                      )
        )
      else throwIO $ ErrorCall "JWT token creation failed"
    )
