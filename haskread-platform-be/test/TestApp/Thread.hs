{-# LANGUAGE OverloadedStrings #-}

module TestApp.Thread (threadAPITests,voteThreadAPITests) where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types
import Test.Tasty
import Test.Tasty.Wai
import TestApp.SampleData
import Servant (Application)

voteThreadAPITests :: Application -> [BSL.ByteString] -> TestTree
voteThreadAPITests app userTokens = testGroup "vote thread api" [
  testVoteThreadAPI app (Prelude.head userTokens)
 ]

threadAPITests :: Application -> [BSL.ByteString] -> TestTree
threadAPITests app userTokens =
  testGroup
    "Thread API Tests"
    [ testCreateThreadAPI app (Prelude.head userTokens),
      testUpdateThreadAPI app (Prelude.head userTokens),
      testDeleteThreadAPI app (Prelude.head userTokens)
    ]

testVoteThreadAPI :: Application -> BSL.ByteString -> TestTree
testVoteThreadAPI app token = do
  testWai app "/upvote - 200" $ do
    res <-
      srequest
        (
          buildRequestWithHeaders
          POST
          "/api/v1/user/thread/upvote/2"
          ""
          [ ("Content-Type", "application/json"),
            (hAuthorization,"Bearer " <> BSL.toStrict token)
          ]
        )
    assertStatus' status200 res

testCreateThreadAPI :: Application -> BSL.ByteString -> TestTree
testCreateThreadAPI app token = do
  testWai app "/create-thread - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            POST
            "/api/v1/user/thread/create"
            threadCreateReqBody
            [ 
              (hAuthorization, "Bearer " <> BSL.toStrict token),
              (hContentType, "multipart/form-data; boundary=------------------------boundary123456789")
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
            [               (hContentType, "multipart/form-data; boundary=------------------------boundary123456789"),
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
