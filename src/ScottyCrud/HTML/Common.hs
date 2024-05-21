{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.HTML.Common where

import           Prelude hiding (head, id, div)
import           Text.Blaze.Html5  as H hiding (map)
import           Text.Blaze.Html5.Attributes as HA hiding (title)
import           ScottyCrud.Common.Types
import qualified ScottyCrud.Common.Types as U (User (..))
import           Data.Text (Text)

headerBar :: Maybe User -> Text -> Html
headerBar mUser titleText = do
    head $ do
      meta ! charset "UTF-8"
      meta ! content "width=device-width, initial-scale=1.0" ! name "viewport"
      script ! src "https://cdn.tailwindcss.com" $ ""
      link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
      title $ toMarkup titleText
    body $ do
      div ! class_ "navbar flex justify-between items-center p-4 bg-indigo-800 text-white" $ do
        div ! class_ "navbar-left flex items-center" $ do
          H.a ! href "/" $ do
            "Scotty CRUD"
        div ! class_ "navbar-center space-x-2.5" $ do
          H.form ! method "GET" ! action "/search" $ do
            input ! class_ "border p-2 rounded text-gray-700" ! placeholder "Search..." ! type_ "search" ! name "search_term"
            button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "submit" $ "Search"
        div ! class_ "navbar-right space-x-0.5" $ do
          case mUser of
            Nothing -> do
              a ! href "/login" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded " ! type_ "button" $ "Login"
              a ! href "/signup" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "button" $ "Signup"
            Just user -> do
              a ! href "/" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "button" $ toHtml (U.userName user)
              a ! href "/logout" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "button" $ "Logout"
              a ! href "/addPost" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "button" $ "addPost"

footerBar :: Html
footerBar = do
    script ! src "/static/style.js" $ ""
    div ! class_ "footer-bar bg-gray-800 text-white py-4 px-8" $ do
      p ! class_ "text-center text-sm" $ do
        "Haskell project for learning."
