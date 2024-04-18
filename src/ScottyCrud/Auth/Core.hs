{-# LANGUAGE OverloadedStrings #-}

module ScottyCrud.Auth.Core where
import           Web.Scotty
import           ScottyCrud.Auth.Handler

authController :: ScottyM ()
authController = do
  get  "/signup"         getSignupR
  get  "/login"          getLoginR
  post "/signupUser"     postSignupUserR
  post "/loginUser"      postLoginUserR
  get  "/logout"         getLogoutR
  put  "/password_reset" putPasswordResetR 
