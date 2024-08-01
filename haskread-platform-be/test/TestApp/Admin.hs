{-# LANGUAGE OverloadedStrings #-}

module TestApp.Admin (adminAPITests) where

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types
import Servant
import Test.Tasty
import Test.Tasty.Wai
import TestApp.SampleData

adminAPITests :: Application -> [BSL.ByteString] -> TestTree
adminAPITests app [tokenBatman,tokenSuperman] =
  testGroup
    "Admin API tests"
    [ testUpdateAdminPassword app tokenSuperman,
      testCreateAdmin app tokenBatman
    ]
adminAPITests _ _ = error "adminAPITests: admin tokens not found"

testUpdateAdminPassword :: Application -> BSL.ByteString -> TestTree
testUpdateAdminPassword app token = do
  testWai app "PUT /update admin - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          PUT
          "/api/v1/admin/profile/change-password"
          adminChangePasswordBody
          [ ("Content-Type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ]
    assertStatus' status200 res

testCreateAdmin :: Application -> BSL.ByteString -> TestTree
testCreateAdmin app token = do
  testWai app "POST /create admin - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          POST
          "/api/v1/admin/create-admin"
          adminCreateAdminReqBody
          [ ("Content-type", "application/json"),
            (hAuthorization, "Bearer " <> BSL.toStrict token)
          ]
    assertStatus' status200 res
