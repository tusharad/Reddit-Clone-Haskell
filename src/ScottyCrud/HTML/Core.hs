{-# LANGUAGE OverloadedStrings #-}
module ScottyCrud.HTML.Core where

import           Prelude hiding (head, id, div)
import           Text.Blaze.Html5  as H hiding (map)
import           Text.Blaze.Html5.Attributes as HA hiding (title)
import           ScottyCrud.Common.Types
import qualified ScottyCrud.Common.Types as PU (PostAndUserAndCat(..))
import           Data.Time.Clock
import           ScottyCrud.HTML.Common
import           ScottyCrud.HTML.Comment

homePage :: Maybe User -> [PostAndUserAndCat] -> Markup
homePage mUser postList = html $ do
  headerBar mUser "ScottyCrud - Home Page"
  body $ do
   div ! class_ "main" $ do
    div ! class_ "container mx-auto my-8" $ do
      div ! class_ "flex flex-wrap" $ do
        div ! class_ "w-full md:w-5/6 px-4" $ do
          H.span $ mapM_ (\post -> div $ do
              div ! class_ "block mb-6 p-4 hover:bg-blue-200 rounded-lg border border-gray-200 bg-white post-item flex justify-between" $ do
                a ! class_ "" ! href ("/viewPost/" <> stringValue (show $ PU.postId post)) $ do
                 h5 ! class_ "text-lg font-semibold text-gray-800" $ toMarkup $ PU.postTitle post
                div ! class_ "text-right text-gray-600" $ do
                  strong (toMarkup $ PU.categoryName post) >> br
                  small  (toMarkup $ show $ utctDay $ PU.createdAt post) >> br
                  small  (toMarkup $ PU.userUserEmail post) >> br
                  case mUser of
                    Nothing -> mempty
                    Just user -> (if user_id user == PU.userId post then div ! class_ "space-x-0.5" $ do
                      a ! href ("/deletePost/" <> toValue (show $ PU.postId post)) $ button "delete"
                      a ! href ("/updatePost/" <> toValue (show $ PU.postId post)) $ button "edit" else mempty)
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

updatePostPage :: Maybe User -> PostAndUserAndCat -> Markup
updatePostPage mUser postInfo = html $ do
  headerBar mUser "ScottyCRUD - Update Post Page"
  body $ do
   div ! class_ "main" $ do
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
                textarea ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" ! HA.name "post_description" $ toMarkup (PU.postDescription postInfo)
              div ! class_ "mb-6" $ do
                button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline w-full" ! type_ "submit" $ "Add Post"
    footerBar

addPostPage :: Maybe User -> Markup
addPostPage mUser = html $ do
  headerBar mUser "ScottyCrud - Add Post Page"
  body $ do
   div ! class_ "main" $ do
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

viewPost :: Maybe User -> PostAndUserAndCat -> [CommentAndUser] -> Markup
viewPost mUser postInfo commentList = html $ do
  headerBar mUser "ScottyCrud - View Post"
  body $ do
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
              "Posted by : " <> toMarkup (PU.userUserEmail postInfo)
            p $ do
              "on "
              em $ do
                toMarkup $ show $ utctDay $ PU.createdAt postInfo
      addCommentForm mUser postInfo
      viewComments mUser commentList
      footerBar

