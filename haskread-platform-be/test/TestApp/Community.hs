{-# LANGUAGE OverloadedStrings #-}
module TestApp.Community (communityAPITests) where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types
import Servant
import Test.Tasty
import Test.Tasty.Wai
import TestApp.SampleData
import Control.Monad.IO.Class (liftIO)

communityAPITests :: Application -> [BSL.ByteString] -> TestTree
communityAPITests app [adminBatmanToken,adminSuperMan] =
  testGroup
    "community APIs"
    [ testCreateCommunityAPI app adminBatmanToken,
      testUpdateCommunityAPI app adminBatmanToken,
      testDeleteCommunityAPI app adminBatmanToken
    ]
communityAPITests _ _ = error "Invalid number of arguments" -- This is a compile-time error

testCreateCommunityAPI :: Application -> BSL.ByteString -> TestTree
testCreateCommunityAPI app token =
  testWai app "POST /create community - 200" $ do
    res <-
      srequest
        (buildRequestWithHeaders
          POST
          "/api/v1/admin/community/create"
          communityCreateReqBody
          [ ("Content-Type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ])
    assertStatus' status200 res

testUpdateCommunityAPI :: Application -> BSL.ByteString -> TestTree
testUpdateCommunityAPI app token =
  testWai app "PUT /update community - 200" $ do
    res <-
      srequest
        (buildRequestWithHeaders
          PUT
          "/api/v1/admin/community/update"
          communityUpdateReqBody
          [ ("Content-Type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ])
    assertStatus' status200 res

testDeleteCommunityAPI :: Application -> BSL.ByteString -> TestTree
testDeleteCommunityAPI app token =
  testWai app "DELETE /delete community - 200" $ do
    res <-
      srequest
        (buildRequestWithHeaders
          DELETE
          "/api/v1/admin/community/delete/4"
          ""
          [ ("Content-Type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ])
    assertStatus' status200 res