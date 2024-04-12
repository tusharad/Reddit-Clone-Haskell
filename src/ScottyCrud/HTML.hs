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


headerBar :: Maybe User -> Html
headerBar mUser = do
    nav ! class_ "header-bar navbar navbar-expand-lg navbar-dark" $ do
      div ! class_ "container-fluid" $ do
        a ! class_ "navbar-brand" ! href "/" $ "ScottyCrud" 
      div ! class_ "collapse navbar-collapse" $ do
        H.form ! class_ "d-flex ms-auto my-lg-0" $ do
            input ! class_ "form-control me-2" ! type_ "search" ! placeholder "Search"
            button ! class_ "btn btn-outline-light" ! type_ "submit" $ "Search"
        case mUser of
          Nothing -> do
            a ! href "/login" $ do
              button ! class_ "btn btn-outline-light ms-2" ! type_ "button" $ "Login"
            a ! href "/signup" $ do
              button ! class_ "btn btn-outline-light ms-2" ! type_ "button" $ "Signup"
          Just user -> do
            a ! href "/" $ do
              button ! class_ "btn btn-outline-light ms-2" ! type_ "button" $ toHtml (user_email user)
            a ! href "/logout" $ do
              button ! class_ "btn btn-outline-light ms-2" ! type_ "button" $ "Logout"
            a ! href "/addPost" $ do
              button ! class_ "btn btn-outline-light ms-2" ! type_ "button" $ "addPost"
 
footerBar :: Html
footerBar = do
      div ! class_ "footer-bar" $ do
        p "Footer here"

signUpPage :: Maybe T.Text -> Markup
signUpPage mMsg = html $ do
  head $ do
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Signup Page"
  body $ do
   div ! class_ "main" $ do
    headerBar Nothing
    div ! class_ "container" $ do
        case mMsg of
          Just msg -> h1 $ toMarkup msg
          Nothing  -> mempty 
        div ! class_ "login-container" $ do
            h2 ! class_ "text-center" $ "Signup"
            H.form ! action "/signupUser" ! method "POST" $ do
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label"  $ "Email address"
                    input ! type_ "email" ! class_ "form-control" ! HA.name "email"
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Password"
                    input ! type_ "password" ! class_ "form-control" ! HA.name "password"
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Confirm Password"
                    input ! type_ "password" ! class_ "form-control" ! HA.name "confirm_password"
                button ! type_ "submit" ! class_ "btn btn-primary w-100" $ "Signup"
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""
    script ! src "/static/style.js" $ ""

loginPage :: Maybe T.Text -> Markup
loginPage mMsg = html $ do
  head $ do
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Login Page"
  body $ do
   div ! class_ "main" $ do
    headerBar Nothing
    div ! class_ "container" $ do
        div ! class_ "login-container" $ do
            h2 ! class_ "text-center" $ "Login"
            case mMsg of
              Just msg -> h1 $ toMarkup msg
              Nothing  -> mempty 
            H.form ! action "/loginUser" ! method "POST" $ do
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Email address"
                    input ! type_ "email" ! class_ "form-control" ! HA.name "email"
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Password"
                    input ! type_ "password" ! class_ "form-control" ! HA.name "password"
                button ! type_ "submit" ! class_ "btn btn-primary w-100" $ "Login"
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""
    script ! src "/static/style.js" $ ""

homePage :: Maybe User -> [PostAndUserAndCat] -> Markup
homePage mUser postList = html $ do
  head $ do
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Login Page"
  body $ do
   div ! class_ "main" $ do
    headerBar mUser
    div ! class_ "container my-4" $ do
      div ! class_ "row" $ do
        div ! class_ "col-md-10" $ do
            H.span $ mapM_ (\post -> div $ do
              a ! href ("/viewPost/" <> stringValue (show $ PU.postId post)) $ do
                div ! class_ "post-item" $ do
                  (h5 $ toMarkup $ PU.postTitle post)
                  (p $ do
                    strong $ toMarkup $ PU.categoryName post
                    br
                    small $ toMarkup $ show $ utctDay $ PU.createdAt post
                    br
                    small $ toMarkup $ PU.userUserEmail post
                    (div $ do
                      case mUser of
                        Nothing   -> mempty
                        Just user -> case (PU.userId post) == (user_id user) of
                            True -> (a ! href ("/deletePost/" <> toValue (show $ PU.postId post)) $ "delete post") >>
                                    br >>
                                    (a ! href ("/updatePost/" <> toValue (show $ PU.postId post)) $ "update post")
                            False -> mempty))
                    ) postList
        div ! class_ "col-md-2" $ do
            div ! class_ "card" $ do
                div ! class_ "card-header" $ do
                    "Featured Posts"
                ul ! class_ "list-group list-group-flush" $ do
                    li ! class_ "list-group-item" $ "Special Post 1"
                    li ! class_ "list-group-item" $ "Special Post 2"
                    li ! class_ "list-group-item" $ "Special Post 3"
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""
    script ! src "/static/style.js" $ ""

updatePostPage :: Maybe User -> PostAndUserAndCat -> Markup
updatePostPage mUser postInfo = html $ do
  head $ do
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
    title "ScottyCrud - Login Page"
  body $ do
   div ! class_ "main" $ do
    headerBar mUser
    div ! class_ "container" $ do
        div ! class_ "login-container" $ do
            h2 ! class_ "text-center" $ "Update Post"
            H.form ! action "/updatePost" ! method "POST" $ do
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Category"
                    select ! HA.name "category_id" $ do
                      option ! HA.value "1" $ "Haskell"
                      option ! HA.value "2" $ "Android"
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Post title"
                    input ! type_ "text" ! class_ "form-control" ! HA.name "post_title" ! HA.value (toValue (PU.postTitle postInfo))
                    input ! type_ "hidden" ! class_ "form-control" ! HA.name "post_id" ! HA.value (toValue (PU.postId postInfo))
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Post Description"
                    textarea ! class_ "form-control" ! rows "3" ! HA.name "post_description" $ (toMarkup $ PU.postDescription postInfo)
               button ! type_ "submit" ! class_ "btn btn-primary w-100" $ "Update Post"
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""
    script ! src "/static/style.js" $ ""


addPostPage :: Maybe User -> Markup
addPostPage mUser = html $ do
  head $ do
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
    title "ScottyCrud - Login Page"
  body $ do
   div ! class_ "main" $ do
    headerBar mUser
    div ! class_ "container" $ do
        div ! class_ "login-container" $ do
            h2 ! class_ "text-center" $ "Add Post"
            H.form ! action "/addPost" ! method "POST" $ do
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Category"
                    select ! HA.name "category_id" $ do
                      option ! HA.value "1" $ "Haskell"
                      option ! HA.value "2" $ "Android"
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Post title"
                    input ! type_ "text" ! class_ "form-control" ! HA.name "post_title"
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Post Description"
                    textarea ! class_ "form-control" ! rows "3" ! HA.name "post_description" $ ""
               button ! type_ "submit" ! class_ "btn btn-primary w-100" $ "Add Post"
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""
    script ! src "/static/style.js" $ ""

viewPost :: Maybe User -> PostAndUserAndCat -> [CommentAndUser] -> Markup
viewPost mUser postInfo commentList = html $ do
  head $ do
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
    title "ScottyCrud - Login Page"
  body $ do
    headerBar mUser
    div ! class_ "container" $ do
      div ! class_ "post-container" $ do
        h2 $ toMarkup $ PU.postTitle postInfo
        div $ do
          p "Posted by " <> (toMarkup $ PU.userUserEmail postInfo)
          p $ do
            "on" 
            em (toMarkup $ show $ utctDay $ PU.createdAt postInfo)
        p "" 
        p (toMarkup $ PU.postDescription postInfo)
      H.form ! action "/addComment" ! method "POST" $ do
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Add Comment"
                    input ! type_ "text" ! class_ "form-control" ! HA.name "comment_content"
                    input ! type_ "hidden" ! class_ "form-control" ! HA.name "post_id" ! HA.value (stringValue (show (PU.postId postInfo)))
                case mUser of
                  Nothing -> button ! type_ "submit" ! class_ "btn btn-primary w-30 disabled" $ "Add Comment"
                  _       -> button ! type_ "submit" ! class_ "btn btn-primary w-30" $ "Add Comment"
                
    div ! class_ "comments-container" $ do
        h3 "Comments" 
        div  $ do
            div ! class_ "comment" $ mapM_ (\comment -> p $ do
              strong $ toMarkup (userEmail comment)
              H.span " : "
              i ! onclick "toggle()" $ "click me"
              div ! class_ "replyBox" $ do
                H.form ! action "/addComment" ! method "POST" $ do
                  div ! class_ "mb-3" $ do
                      H.label ! class_ "form-label" $ "Add Comment"
                      input ! type_ "text" ! class_ "form-control" ! HA.name "comment_content"
                      input ! type_ "hidden" ! class_ "form-control" ! HA.name "post_id" ! HA.value (stringValue (show (PU.postId postInfo)))
                  case mUser of
                    Nothing -> button ! type_ "submit" ! class_ "btn btn-primary w-30 disabled" $ "Add Comment"
                    _       -> button ! type_ "submit" ! class_ "btn btn-primary w-30" $ "Add Comment"
              toMarkup (commentContent comment)) commentList
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""
    script ! src "/static/style.js" $ ""
