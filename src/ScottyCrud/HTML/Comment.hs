{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module ScottyCrud.HTML.Comment where

import           Prelude hiding (head, id, div)
import           Text.Blaze.Html5  as H hiding (map)
import           Text.Blaze.Html5.Attributes as HA hiding (title)
import           ScottyCrud.Common.Types
import qualified ScottyCrud.Common.Types as PU (PostAndUserAndCat(..))
import qualified ScottyCrud.Common.Types as CU (CommentAndUser(..))
import           Data.Maybe
import           ScottyCrud.HTML.Common
import           Data.String.Interpolate as SI (i) 

isAuthor :: Maybe User -> CommentAndUser -> Bool
isAuthor Nothing _ = False
isAuthor (Just user) c = user_id user == CU.userId c

deleteCommentButton :: CommentAndUser -> Markup
deleteCommentButton comment = div $ do
  H.form ! method "POST" ! action "/deleteComment" $ do
          input ! type_ "hidden" ! HA.name "comment_id" ! HA.value (stringValue (show (commentId comment)))
          button ! type_ "submit" ! class_ 
            "bg-yellow-500 hover:bg-yellow-700 text-black font-bold py-1 px-2 rounded-r"  $ "delete"

editCommentButton :: Markup
editCommentButton = button ! class_ 
  "bg-green-500 hover:bg-green-700 text-black font-bold py-1 px-2 rounded-r" ! id "commentEditButton" $ "edit"

hashSym :: String
hashSym = "#" 

editCommentForm :: Markup
editCommentForm = do
  H.style $ [SI.i| 
      #{hashSym}editForm {
            display: none;
            margin-top: 10px;
        }
  |]
  div ! id "commentEditForm" $ do
    H.form ! method "POST" ! action "/editComment" $ do
      H.input ! class_ "form-control border" ! type_ "text"
      button ! type_ "submit" ! class_ "bg-purple-500 hover:bg-purple-700 text-black font-bold py-1 px-2 rounded-r" $ "submit"

viewComment :: Maybe User -> NestedComment -> Markup
viewComment mUser nestedComment = do
  div ! class_ "pl-8 mt-4" $ do
    div ! class_ "p-6 rounded-lg shadow-md flex justify-between items-start" $ do
        div $ do
          strong $ toMarkup (CU.userName (mainComment nestedComment)) <> " : "
          toMarkup (commentContent (mainComment nestedComment))
          replyForm (mainComment nestedComment)
        div ! class_ "text-right" $ do
          if (isAuthor mUser $ mainComment nestedComment) then deleteCommentButton (mainComment nestedComment) >> editCommentButton >> editCommentForm else mempty
        div $ mapM_ (viewComment mUser) (childComments nestedComment)

replyForm :: CommentAndUser -> Html
replyForm comment =
  H.form ! action "/addComment" ! method "POST" $ do
              input ! type_ "text"   ! class_ "border" ! HA.name "comment_content"
              input ! type_ "hidden" ! class_ "form-control" ! HA.name "post_id" ! HA.value (stringValue (show (CU.postId comment)))
              input ! type_ "hidden" ! class_ "form-control" ! HA.name "parent_comment_id" ! HA.value (stringValue (show (commentId comment)))
              button ! class_ "center bg-gray-300 hover:bg-gray-400 text-gray-800 rounded " $ "Reply"

viewComments :: Maybe User -> [CommentAndUser] -> Html
viewComments mUser commentList = do
 div ! class_ "" $ do
      h3 ! class_ "font-bold text-xl mb-4" $ "Comments"
      div ! class_ "pl-4" $ do
        div ! class_ "bg-white p-4 rounded-lg shadow-md mb-4" $ mapM_ (\comment -> div $ do viewComment mUser comment) (toNestedComment commentList)

addCommentForm :: Maybe User -> PostAndUserAndCat -> Markup
addCommentForm mUser postInfo = do
  div ! class_ "" $ do
    H.form ! action "/addComment" ! method "POST" ! class_ "mt-6 space-x-0.5" $ do
          div ! class_ "mb-4 space-x-2.5" $ do
            H.label ! class_ "block text-gray-700 text-sm font-bold mb-2 " $ do
              "Add Comment"
            input ! class_ "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" ! name "comment_content" ! type_ "text"
            input ! name "post_id" ! type_ "hidden" ! HA.name "post_id" ! HA.value (stringValue (show (PU.postId postInfo)))
            case mUser of
                    Nothing -> button ! class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline disabled" ! type_ "submit" $ "Add Comment"
                    _       -> button ! class_ btnCSS ! type_ "submit" $ "Add Comment"

toNest :: [CommentAndUser] -> CommentAndUser -> NestedComment
toNest commentList c = NestedComment c (map (toNest commentList) children)
  where
    children = filter (\comment -> Just (commentId c) == parentCommentId comment) commentList

toNestedComment :: [CommentAndUser] -> [NestedComment]
toNestedComment commentList = map (toNest commentList) rootComments
  where
    rootComments = filter (isNothing.parentCommentId) commentList
