{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ScottyCrud.HTML where

import           Prelude hiding (head, id, div)
import           Text.Blaze.Html5  as H hiding (map) 
import           Text.Blaze.Html5.Attributes as HA hiding (title)
import           ScottyCrud.Common.Types
import qualified ScottyCrud.Common.Types as PU (PostAndUserAndCat(..))
import           Data.Time.Clock
import qualified Data.Text as T
import           Data.Maybe

headerBar :: Maybe User -> Html
headerBar mUser = do
    head $ do
      meta ! charset "UTF-8"
      meta ! content "width=device-width, initial-scale=1.0" ! name "viewport"
      script ! src "https://cdn.tailwindcss.com" $ ""
    body $ do 
      div ! class_ "navbar flex justify-between items-center p-4 bg-indigo-800 text-white" $ do
        div ! class_ "navbar-left flex items-center" $ do
          H.a ! href "/" $ do
            "Scotty CRUD"
        div ! class_ "navbar-center space-x-2.5" $ do
          H.form ! method "GET" ! action "search" $ do
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
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "button" $ toHtml (user_email user)
              a ! href "/logout" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "button" $ "Logout"
              a ! href "/addPost" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ! type_ "button" $ "addPost"
 
footerBar :: Html
footerBar = do
    div ! class_ "footer-bar bg-gray-800 text-white py-4 px-8" $ do
      p ! class_ "text-center text-sm" $ do
        "Haskell project for learning."

signUpPage :: Maybe T.Text -> Markup
signUpPage mMsg = html $ do
  head $ do
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Signup Page"
  body $ do
   div ! class_ "main" $ do
    headerBar Nothing
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
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "email" $ do
                  "Email address"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! name "email" ! type_ "email" ! HA.name "email"
              div ! class_ "mb-6" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "password" $ do
                  "Password"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" ! name "password" ! type_ "password" ! HA.name "password"
              div ! class_ "mb-6" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" ! for "password" $ do
                  "Confirm Password"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" ! name "password" ! type_ "password" ! HA.name "confirm_password"
              div ! class_ "flex items-center justify-between" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline w-full" ! type_ "submit" $ "Signup"
    footerBar
    script ! src "/static/style.js" $ ""

loginPage :: Maybe T.Text -> Markup
loginPage mMsg = html $ do
  head $ do
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Login Page"
  body $ do
   div ! class_ "main" $ do
    headerBar Nothing
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
    script ! src "/static/style.js" $ ""


homePage :: Maybe User -> [PostAndUserAndCat] -> Markup
homePage mUser postList = html $ do
  head $ do
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Home Page"
  body $ do
   div ! class_ "main" $ do
    headerBar mUser
    div ! class_ "container mx-auto my-8" $ do
      div ! class_ "flex flex-wrap" $ do
        div ! class_ "w-full md:w-5/6 px-4" $ do
          H.span $ mapM_ (\post -> div $ do
              div ! class_ "block mb-6 p-4 hover:bg-blue-200 rounded-lg border border-gray-200 bg-white post-item flex justify-between" $ do
                a ! class_ "" ! href ("/viewPost/" <> stringValue (show $ PU.postId post)) $ do
                 h5 ! class_ "text-lg font-semibold text-gray-800" $ do
                  toMarkup $ PU.postTitle post
                div ! class_ "text-right text-gray-600" $ do
                  strong $ do
                    toMarkup $ PU.categoryName post
                  br 
                  small $ do
                    toMarkup $ show $ utctDay $ PU.createdAt post
                  br 
                  small $ toMarkup $ PU.userUserEmail post
                  br
                  case mUser of
                    Nothing -> mempty
                    Just user -> case (user_id user) == PU.userId post of
                      False -> mempty
                      True -> div ! class_ "space-x-0.5" $ do
                        a ! href ("/deletePost/" <> toValue (show $ PU.postId post)) $ button $ "delete"
                        a ! href ("/updatePost/" <> toValue (show $ PU.postId post)) $ button $ "edit"
                  ) postList
        div ! class_ "w-full md:w-1/6 px-4" $ do
          div ! class_ "card bg-white shadow-lg rounded-lg" $ do
            div ! class_ "card-header py-3 px-4 font-semibold text-white bg-blue-500 rounded-t-lg" $ do
              "Featured Posts"
            ul ! class_ "list-group list-group-flush" $ do
              li ! class_ "list-group-item px-4 py-2 border-b border-gray-200" $ do
                "Special Post 1"
              li ! class_ "list-group-item px-4 py-2 border-b border-gray-200" $ do
                "Special Post 2"
              li ! class_ "list-group-item px-4 py-2" $ do
                "Special Post 3"
    footerBar
    script ! src "/static/style.js" $ ""

updatePostPage :: Maybe User -> PostAndUserAndCat -> Markup
updatePostPage mUser postInfo = html $ do
  head $ do
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Update Post Page"
  body $ do
   div ! class_ "main" $ do
    headerBar mUser
    div ! class_ "container mx-auto mt-8" $ do
        div ! class_ "container mx-auto mt-8" $ do
          div ! class_ "w-full max-w-xs mx-auto" $ do
            h2 ! class_ "text-center text-2xl font-bold mb-6" $ do
              "Add Post"
            H.form ! action "/addPost" ! class_ "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ! method "POST" $ do
              div ! class_ "mb-4" $ do
                select ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! HA.name "category_id" $ do
                  option ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! type_ "text" ! HA.value "1" $ "haskell"
                  option ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! type_ "text" ! HA.value "2" $ "android"
              div ! class_ "mb-4" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" $ "Post Title"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! type_ "text" ! HA.name "post_title" ! HA.value (toValue (PU.postTitle postInfo))
              input ! type_ "hidden"  ! HA.name "post_id" ! HA.value (toValue (PU.postId postInfo))
              div ! class_ "mb-6" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" $ do "Post Description"
                textarea ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" ! HA.name "post_description" $ (toMarkup $ PU.postDescription postInfo)
              div ! class_ "mb-6" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline w-full" ! type_ "submit" $ "Add Post"
    footerBar
    script ! src "/static/style.js" $ ""


addPostPage :: Maybe User -> Markup
addPostPage mUser = html $ do
  head $ do
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Add Post Page"
  body $ do
   div ! class_ "main" $ do
    headerBar mUser
    div ! class_ "container mx-auto mt-8" $ do
        div ! class_ "container mx-auto mt-8" $ do
          div ! class_ "w-full max-w-xs mx-auto" $ do
            h2 ! class_ "text-center text-2xl font-bold mb-6" $ do
              "Add Post"
            H.form ! action "/addPost" ! class_ "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ! method "POST" $ do
              div ! class_ "mb-4" $ do
                select ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! HA.name "category_id" $ do
                  option ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! type_ "text" ! HA.value "1" $ "haskell"
                  option ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! type_ "text" ! HA.value "2" $ "android"
              div ! class_ "mb-4" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" $ "Post Title"
                input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! type_ "text" ! HA.name "post_title"
              div ! class_ "mb-6" $ do
                H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" $ do "Post Description"
                textarea ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" ! HA.name "post_description" $ ""
              div ! class_ "mb-6" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline w-full" ! type_ "submit" $ "Add Post"
    footerBar
    script ! src "/static/style.js" $ ""

viewPost :: Maybe User -> PostAndUserAndCat -> [CommentAndUser] -> Markup
viewPost mUser postInfo commentList = html $ do
  head $ do
    title "ScottyCrud - View Post"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
  body $ do
    headerBar mUser
    div ! class_ "container" $ do
      div ! class_ "container mx-auto p-4" $ do
        div ! class_ "bg-white p-6 rounded-lg shadow-md flex justify-between items-start" $ do
          div $ do
            h2 ! class_ "font-bold text-xl mb-4" $ do
              toMarkup $ PU.postTitle postInfo
            div ! class_ "p-4 border rounded-lg bg-green-100 w-full" $ do
              p $ do
                toMarkup $ PU.postDescription postInfo
          div ! class_ "text-right text-sm" $ do
            p $ do
              "Posted by : " <> (toMarkup $ PU.userUserEmail postInfo)
            p $ do
              "on "
              em $ do
                (toMarkup $ show $ utctDay $ PU.createdAt postInfo)
      addCommentForm mUser postInfo
      viewComments mUser postInfo commentList
      footerBar
      script ! src "/static/style.js" $ ""

viewComments mUser postInfo commentList = do
  div ! class_ "" $ do
      h3 ! class_ "font-bold text-xl mb-4" $ do
        "Comments"
      div ! class_ "pl-4" $ do
        div ! class_ "bg-white p-4 rounded-lg shadow-md mb-4" $ mapM_ (\comment -> div $ do
          div $ do
            p $ do
              strong $ do
                toMarkup (userEmail (mainComment comment)) <> " : "
              toMarkup (commentContent (mainComment comment))
            H.form ! action "/addComment" ! method "POST" $ do
              input ! type_ "text" ! class_ "border" ! HA.name "comment_content"
              input ! type_ "hidden" ! class_ "form-control" ! HA.name "post_id" ! HA.value (stringValue (show (PU.postId postInfo)))
              input ! type_ "hidden" ! class_ "form-control" ! HA.name "parent_comment_id" ! HA.value (stringValue (show (commentId (mainComment comment))))
              button ! class_ "bg-gray-300 hover:bg-gray-400 text-gray-800 rounded " $ "Reply"
            mapM_ (\childComment -> div $ do
              div ! class_ "pl-8 mt-4" $ do
                div ! class_ "bg-white p-4 rounded-lg shadow-md mb-4" $ do
                  p $ do
                    strong $ do
                      toMarkup (userEmail (mainComment childComment))
                    toMarkup (commentContent (mainComment childComment))) (childComments comment)
                ) (toNestedComment commentList)


addCommentForm :: Maybe User -> PostAndUserAndCat -> Markup
addCommentForm mUser postInfo = do
  H.form ! action "/addComment" ! method "POST" ! class_ "mt-6 space-x-0.5" $ do
          div ! class_ "mb-4 space-x-2.5" $ do
            H.label ! class_ "block text-gray-700 text-sm font-bold mb-2" $ do
              "Add Comment"
            input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! name "comment_content" ! type_ "text"
            input ! name "post_id" ! type_ "hidden" ! HA.name "post_id" ! HA.value (stringValue (show (PU.postId postInfo)))
            case mUser of
                    Nothing -> button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline disabled" ! type_ "submit" $ "Add Comment"
                    _       -> button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline" ! type_ "submit" $ "Add Comment"

data NestedComment = NestedComment {
    mainComment :: CommentAndUser
  , childComments :: [NestedComment]
} deriving (Eq,Show)

toNestedComment :: [CommentAndUser] -> [NestedComment]
toNestedComment [] = []
toNestedComment (comment:commentList) = NestedComment comment (toNestedComment resultList) : toNestedComment commentList
  where
    resultList = filter (\x -> (commentId comment) == fromMaybe (-1) (parentCommentId x)) commentList

printComments :: Maybe User -> [CommentAndUser] -> PostAndUserAndCat -> Markup
printComments mUser commentList postInfo = div ! class_ "comment" $ mapM_ (\comment -> p $ do
              strong $ toMarkup (userEmail comment)
              H.span " : "
              toMarkup (commentContent comment)
              button ! onclick "toggle()" $ "Reply"
              div ! class_ "replyBox" $ do
                H.form ! action "/addComment" ! method "POST" $ do
                  div ! class_ "mb-3" $ do
                      H.label ! class_ "form-label" $ "Add Comment"
                      input ! type_ "text" ! class_ "form-control" ! HA.name "comment_content"
                      input ! type_ "hidden" ! class_ "form-control" ! HA.name "parent_comment_id" ! HA.value (stringValue (show $ commentId comment))
                      input ! type_ "hidden" ! class_ "form-control" ! HA.name "post_id" ! HA.value (stringValue (show (PU.postId postInfo)))
                  case mUser of
                    Nothing -> button ! type_ "submit" ! class_ "btn btn-primary w-30 disabled" $ "Add Comment"
                    _       -> button ! type_ "submit" ! class_ "btn btn-primary w-30" $ "Add Comment"
              ) commentList

