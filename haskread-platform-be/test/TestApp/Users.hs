{-# LANGUAGE OverloadedStrings #-}

module TestApp.Users where

import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Network.HTTP.Types
import Servant
import Test.Tasty
import Test.Tasty.Wai
import TestApp.SampleData
import Data.List                 (intersperse)

userAPITests :: Application -> [BSL.ByteString] -> TestTree
userAPITests app [token1, token2, token3] =
  testGroup
    "User APIs"
    [ testUserDashboard app token2,
      testUserChangePassword app token1,
      testUserDeleteAccount app token3,
      testUpdateUserProfileImage app token2
    ]
userAPITests _ _ = testGroup "Servant HaskRead" []

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

testUpdateUserProfileImage :: Application -> BSL.ByteString -> TestTree
testUpdateUserProfileImage app token = do
  testWai app "PUT /user/profile/update-image - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          PUT
          "/api/v1/user/profile/update-image"
          correctBody
          (multipartHeaders <> [(hAuthorization, "Bearer " <> BSL.toStrict token)])
    assertStatus' status200 res 

multipartHeaders :: [(HeaderName,ByteString)]
multipartHeaders = [(hContentType, "multipart/form-data; boundary=XX")]

correctBody :: BSL.ByteString
correctBody = mconcat $ intersperse "\n"
  [ "--XX"
  , "Content-Disposition: form-data; name=\"pic\"; filename=\"body.png\""
  , "Content-Type: image/png"
  , ""
  , "Foo body"
  , ""
  , "--XX--"
  ]