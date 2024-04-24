{-# LANGUAGE OverloadedStrings #-}

module ScottyCrud.Auth.Core where
import           Web.Scotty.Trans
import           ScottyCrud.Auth.Handler
import           ScottyCrud.Common.Types

authController :: ScottyT AppM ()
authController = do
  get  "/signup"         getSignupR
  get  "/login"          getLoginR
  post "/signupUser"     postSignupUserR
  post "/loginUser"      postLoginUserR
  get  "/logout"         getLogoutR
  put  "/password_reset" putPasswordResetR 
  get "/verify_email/:uid"    getVerifyEmail
