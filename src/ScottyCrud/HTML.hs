{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ScottyCrud.HTML where

import           Prelude hiding (head, id, div)
import           Text.Blaze.Html5  as H hiding (map) 
import           Text.Blaze.Html5.Attributes as HA hiding (title)
import           ScottyCrud.Common.Types
import qualified ScottyCrud.Common.Types as PU (PostAndUserAndCat(..))
import           Data.Time.Clock


headerBar :: Maybe User -> Html
headerBar mUser = do
    nav ! class_ "header-bar navbar navbar-expand-lg navbar-dark" $ do
      div ! class_ "container-fluid" $ do
        a ! class_ "navbar-brand" ! href "/" $ "ScottyCrud" 
      div ! class_ "collapse navbar-collapse" $ do
        H.form ! class_ "d-flex ms-auto my-lg-0" $ do
           -- a ! href ("/search") $ do
             button ! class_ "btn btn-outline-light" ! type_ "search" $ "Search"
             input ! class_ "form-control me-2" ! type_ "search" ! placeholder "Search"

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

signUpPage :: Markup
signUpPage = html $ do
  head $ do
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    title "ScottyCrud - Signup Page"
  body $ do
   div ! class_ "main" $ do
    headerBar Nothing
    div ! class_ "container" $ do
        div ! class_ "login-container" $ do
            h2 ! class_ "text-center" $ "Signup"
            H.form ! action "/signupUser" ! method "POST" $ do
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label"  $ "Email address"
                    input ! type_ "email" ! class_ "form-control" ! HA.name "email"
                div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Password"
                    input ! type_ "password" ! class_ "form-control" ! HA.name "password"
                button ! type_ "submit" ! class_ "btn btn-primary w-100" $ "Signup"
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""

loginPage :: Markup
loginPage = html $ do
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
                    small $ toMarkup $ PU.userUserEmail post)
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
                    input ! type_ "number" ! class_ "form-control" ! HA.name "category_id"
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Post title"
                    input ! type_ "text" ! class_ "form-control" ! HA.name "post_title"
               div ! class_ "mb-3" $ do
                    H.label ! class_ "form-label" $ "Post Description"
                    textarea ! class_ "form-control" ! rows "3" ! HA.name "post_description" $ ""
               button ! type_ "submit" ! class_ "btn btn-primary w-100" $ "Add Post"
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""

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
              toMarkup (commentContent comment)) commentList
    footerBar
    script ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" $ ""

searchResultsPage :: [Post] -> Markup
searchResultsPage posts = html $ do
    head $ do
      title "Search Results"
    body $ do
        h1 "Search Results"
        div ! class_ "posts" $ do
            mapM_ renderPost posts
  where
    renderPost :: Post -> Markup
    renderPost post = div ! class_ "post" $ do
        h2 $ toMarkup $ PU.postTitle Post
        p $ toMarkup $ PU.postDescription Post


{-
homePage :: Maybe User -> [Post] -> Markup
homePage mUser postList = html $ do
    head $ do
        title "learning haskell" -- <title>learning haskell</title>
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
    body $ do
        case mUser of
          Just user -> p $ "Hello " <> toHtml (user_email user)
          Nothing -> p "please login"
        h1 "Hello from Scotty!"
        h3 "Post List: "
        ol $ mapM_ (\post -> li $ div $ do
          (p $ toMarkup $ postTitle post)
          (p $ toMarkup $ postDescription post)) postList

addPostPage :: Markup
addPostPage = html $ do
  body $ do
    h1 "Add Post"
    Text.Blaze.Html5.form ! action "/addPost" ! method "POST" $ do
      p "Enter title"
      input ! type_ "text" ! Text.Blaze.Html5.Attributes.name "post_title"
      br
      p "Enter post description"
      input ! type_ "text" ! Text.Blaze.Html5.Attributes.name "post_description"
      input ! type_ "submit" ! Text.Blaze.Html5.Attributes.value "submit"

personsPage :: [Person] -> Markup
personsPage lst = html $ do
  body $ do
    h1 "Person list:"
    ul $ mapM_ (li. toMarkup . getName) lst

insertPersonPage :: Markup
insertPersonPage = html $ do
  body $ do
    h1 "Insert Person Form"
    Text.Blaze.Html5.form ! action "/addPerson" ! method "POST" $ do
      p "Enter name"
      input ! type_ "text" ! Text.Blaze.Html5.Attributes.name "name"
      br
      p "Enter age"
      input ! type_ "number" ! Text.Blaze.Html5.Attributes.name "age"
      input ! type_ "submit" ! Text.Blaze.Html5.Attributes.value "submit"
  

signUpPage :: Markup
signUpPage = html $ do
  body $ do
    h1 "Signup"
    Text.Blaze.Html5.form ! action "/signupUser" ! method "POST" $ do
      p "Enter email"
      input ! type_ "email" ! Text.Blaze.Html5.Attributes.name "email"
      br
      p "Enter password"
      input ! type_ "password" ! Text.Blaze.Html5.Attributes.name "password"
      input ! type_ "submit" ! Text.Blaze.Html5.Attributes.value "submit"

loginPage :: Markup
loginPage = html $ do
  body $ do
    h1 "Login"
    Text.Blaze.Html5.form ! action "/loginUser" ! method "POST" $ do
      p "Enter email"
      input ! type_ "email" ! Text.Blaze.Html5.Attributes.name "email"
      br
      p "Enter password"
      input ! type_ "password" ! Text.Blaze.Html5.Attributes.name "password"
      input ! type_ "submit" ! Text.Blaze.Html5.Attributes.value "Login"
-}

getName :: Person -> String
getName Person{..} = name

{-
tempPage :: MarkUp 
tempPage = html $ do
  head $ do
    title "login"
    link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" ! rel "stylesheet"
  body $ do
-}
