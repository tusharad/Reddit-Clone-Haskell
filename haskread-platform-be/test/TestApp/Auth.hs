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
      testAdminLogin app
    ]

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
