{-# LANGUAGE OverloadedStrings #-}

module TestApp.Auth (authAPITests) where

import Network.HTTP.Types
import Servant
import Test.Tasty
import Test.Tasty.Wai
import TestApp.SampleData

authAPITests :: Application -> TestTree
authAPITests app =
  testGroup
    "auth apis"
    [ testUserRegister app,
      testUserLogin app,
      testAdminLogin app,
      testVerifyEmail app,
      testResendVerifyEmail app
    ]

testResendVerifyEmail :: Application -> TestTree
testResendVerifyEmail app =
  testWai app "PUT /user/auth/verify - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          PUT
          "api/v1/user/auth/verify/resend/1"
          ""
          []
    assertStatus' status200 res

testVerifyEmail :: Application -> TestTree
testVerifyEmail app =
  testWai app "PUT /user/auth/verify - 200" $ do
    res <-
      srequest $
        buildRequestWithHeaders
          PUT
          "api/v1/user/auth/verify/2/1234"
          ""
          []
    assertStatus' status200 res

testUserRegister :: Application -> TestTree
testUserRegister app =
  testWai app "POST /register user - 200" $ do
    res <-
      postWithHeaders
        "/api/v1/user/auth/register"
        registerUserBody
        [("Content-Type", "application/json")]
    assertStatus' status200 res

testUserLogin :: Application -> TestTree
testUserLogin app =
  testWai app "POST /login user - 200" $ do
    res <-
      postWithHeaders
        "/api/v1/user/auth/login"
        correctLoginUserBody
        [("Content-Type", "application/json")]
    assertStatus' status200 res

testAdminLogin :: Application -> TestTree
testAdminLogin app =
  testWai app "POST /login admin - 200" $ do
    res <-
      postWithHeaders
        "/api/v1/admin/auth/login"
        loginAdminBody
        [("Content-Type", "application/json")]
    assertStatus' status200 res
