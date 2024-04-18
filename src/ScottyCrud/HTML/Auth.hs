{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.HTML.Auth where

import           Prelude hiding (head, id, div)
import           Text.Blaze.Html5  as H hiding (map)
import           Text.Blaze.Html5.Attributes as HA hiding (title)
import qualified Data.Text as T
import           ScottyCrud.HTML.Common

signUpPage :: Maybe T.Text -> Markup
signUpPage mMsg = html $ do
  headerBar Nothing "ScottyCRUD - Signup Page"
  body $ do
   div ! class_ "main" $ do
    div ! class_ "container mx-auto mt-8" $ do
        case mMsg of
          Just msg -> h1 $ toMarkup msg
          Nothing  -> mempty
        div ! class_ "container mx-auto mt-8" $ do
          div ! class_ "w-full max-w-xs mx-auto" $ do
            h2 ! class_ "text-center text-2xl font-bold mb-6" $ do
              "Signup"
            H.form ! action "/signupUser" ! class_ "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ! method "POST" $ do
              div ! class_ "mb-4" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2"  $ "User name"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! name "userName" ! type_ "text" 
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "email" $ do
                  "Email address"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! type_ "email" ! HA.name "email"
              div ! class_ "mb-6" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "password" $ "Password"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" ! type_ "password" ! HA.name "password"
              div ! class_ "mb-6" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "password" $ "Confirm Password"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline"  ! type_ "password" ! HA.name "confirm_password"
              div ! class_ "flex items-center justify-between" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline w-full" ! type_ "submit" $ "Signup"
    footerBar

loginPage :: Maybe T.Text -> Markup
loginPage mMsg = html $ do
  headerBar Nothing "ScottyCRUD - Login Page"
  body $ do
   div ! class_ "main" $ do
    div ! class_ "container mx-auto mt-8" $ do
        case mMsg of
          Just msg -> h1 $ toMarkup msg
          Nothing  -> mempty
        div ! class_ "container mx-auto mt-8" $ do
          div ! class_ "w-full max-w-xs mx-auto" $ do
            h2 ! class_ "text-center text-2xl font-bold mb-6" $ do
              "Login"
            H.form ! action "/loginUser" ! class_ "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ! method "POST" $ do
              div ! class_ "mb-4" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "email" $ do
                  "Email address"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! name "email" ! type_ "email" ! HA.name "email"
              div ! class_ "mb-6" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "password" $ do
                  "Password"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" ! name "password" ! type_ "password" ! HA.name "password"
              div ! class_ "mb-6" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline w-full" ! type_ "submit" $ "Login"
    footerBar
