{-# LANGUAGE OverloadedStrings #-}

module TestApp.Thread (threadAPITests) where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types
import Servant
import Test.Tasty
import Test.Tasty.Wai
import TestApp.SampleData
import Control.Monad.IO.Class (liftIO)

threadAPITests :: Application -> [BSL.ByteString] -> TestTree
threadAPITests app userTokens =
  testGroup
    "Thread API Tests"
    [ testCreateThreadAPI app (Prelude.head userTokens),
      testUpdateThreadAPI app (Prelude.head userTokens),
      testDeleteThreadAPI app (Prelude.head userTokens)
    ]

testCreateThreadAPI :: Application -> BSL.ByteString -> TestTree
testCreateThreadAPI app token = do
  testWai app "/create-thread - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            POST
            "/api/v1/user/thread/create"
            threadCreateReqBody
            [ ("Content-Type", "application/json"),
              (hAuthorization, "Bearer " <> BSL.toStrict token)
            ]
        )
    assertStatus' status200 res

testUpdateThreadAPI :: Application -> BSL.ByteString -> TestTree
testUpdateThreadAPI app token = do
  testWai app "/update-thread - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            PUT
            "/api/v1/user/thread/update"
            threadUpdateReqBody
            [ ("Content-Type", "application/json"),
              (hAuthorization, "Bearer " <> BSL.toStrict token)
            ]
        )
    assertStatus' status200 res

testDeleteThreadAPI :: Application -> BSL.ByteString -> TestTree
testDeleteThreadAPI app token = do
  testWai app "/delete-thread - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            DELETE
            "/api/v1/user/thread/delete/1"
            ""
            [ ("Content-Type", "application/json"),
              (hAuthorization, "Bearer " <> BSL.toStrict token)
            ]
        )
    assertStatus' status200 res