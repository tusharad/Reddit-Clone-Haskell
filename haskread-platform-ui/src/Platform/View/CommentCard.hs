{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.View.CommentCard
  ( CommentCardOps (..)
  , CommentCardId (..)
  , commentCardView
  , addCommentView
  , disabledAddCommentButtonView
  , addCommentButtonView
  , textarea
  , updateVoteCount
  , updateCurrUserVotes
  , Action (..)
  , update
  , showDislikeIcon
  , showLikeIcon
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text (Text, append)
import qualified Data.Text as T
import Effectful (IOE)
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Web.Hyperbole hiding (input, textarea)
import Web.Hyperbole.View (onInput)

data CommentCardOps = CommentCardOps
  { currUserVotes :: Maybe [(Int, Bool)]
  , tokenForCommentCard :: Maybe Text
  , commentInfo :: CommentInfo
  }
  deriving (Show, Eq, Read)

newtype CommentCardId = CommentCardId Int
  deriving (Show, Read, ViewId)

instance (IOE :> es) => HyperView CommentCardId es where
  data Action CommentCardId
    = LikeComment CommentCardOps
    | DislikeComment CommentCardOps
    | AddCommentBtn AddCommentData
    | SubmitAddComment Text Int (Maybe Int)
    | CancelAddComment Int
    | DoRedirect Int
    | GoToLogin
    deriving (Show, Read, ViewAction)

  update GoToLogin = redirect "/login"
  update (AddCommentBtn AddCommentData {..}) =
    pure $ addCommentView userToken threadId parentCommentIdForAddComment genForm
  update (DoRedirect tId) = redirect . url $ T.pack ("/view-thread/" <> show tId)
  update (CancelAddComment threadId) =
    redirect . url $ T.pack ("/view-thread/" <> show threadId)
  update (SubmitAddComment token tId mParentCommentId) = do
    uf <- formData @AddCommentForm
    let vals = validateForm uf
    if anyInvalid vals
      then
        pure $ addCommentView token tId mParentCommentId vals
      else do
        void . liftIO $
          addComment
            AddCommentData
              { contentForAddComment = commentContentField uf
              , threadId = tId
              , userToken = token
              , parentCommentIdForAddComment = mParentCommentId
              }
        redirect $ url $ "/view-thread/" `append` toText tId
  update (LikeComment commentCardOps@CommentCardOps {..}) = do
    let commentId = commentIDForCommentInfo commentInfo
    liftIO $ upvoteComment tokenForCommentCard commentId
    pure $
      commentCardView
        commentCardOps
          { currUserVotes = updateCurrUserVotes currUserVotes commentId True
          , commentInfo = updateVoteCount currUserVotes True commentInfo
          }
  update (DislikeComment commentCardOps@CommentCardOps {..}) = do
    let commentId = commentIDForCommentInfo commentInfo
    liftIO $ downvoteComment tokenForCommentCard commentId
    pure $
      commentCardView
        commentCardOps
          { currUserVotes = updateCurrUserVotes currUserVotes commentId False
          , commentInfo = updateVoteCount currUserVotes False commentInfo
          }

updateVoteCount :: Maybe [(Int, Bool)] -> Bool -> CommentInfo -> CommentInfo
updateVoteCount Nothing True t =
  t {commentUpvoteCount = Just $ fromMaybe 0 (commentUpvoteCount t) + 1}
updateVoteCount Nothing False t =
  t {commentDownvoteCount = Just $ fromMaybe 0 (commentDownvoteCount t) + 1}
updateVoteCount (Just vals) True t = case lookup (commentIDForCommentInfo t) vals of
  Just True -> t {commentUpvoteCount = Just $ fromMaybe 0 (commentUpvoteCount t) - 1}
  Just False ->
    t
      { commentUpvoteCount = Just $ fromMaybe 0 (commentUpvoteCount t) + 1
      , commentDownvoteCount = Just $ fromMaybe 0 (commentDownvoteCount t) - 1
      }
  Nothing -> t {commentUpvoteCount = Just $ fromMaybe 0 (commentUpvoteCount t) + 1}
updateVoteCount (Just vals) False t = case lookup (commentIDForCommentInfo t) vals of
  Just True ->
    t
      { commentUpvoteCount = Just $ fromMaybe 0 (commentUpvoteCount t) - 1
      , commentDownvoteCount = Just $ fromMaybe 0 (commentDownvoteCount t) + 1
      }
  Just False -> t {commentDownvoteCount = Just $ fromMaybe 0 (commentDownvoteCount t) - 1}
  Nothing -> t {commentDownvoteCount = Just $ fromMaybe 0 (commentDownvoteCount t) + 1}

updateCurrUserVotes :: Maybe [(Int, Bool)] -> Int -> Bool -> Maybe [(Int, Bool)]
updateCurrUserVotes Nothing tId newVote = Just [(tId, newVote)]
updateCurrUserVotes (Just vals) tId newVote = Just $ case lookup tId vals of
  Just True ->
    if newVote
      then filter (\(k, _) -> k /= tId) vals
      else modifyMap vals tId newVote
  Just False ->
    if newVote
      then modifyMap vals tId newVote
      else filter (\(k, _) -> k /= tId) vals
  Nothing -> (tId, newVote) : vals

modifyMap :: [(Int, Bool)] -> Int -> Bool -> [(Int, Bool)]
modifyMap [] tId newVal = [(tId, newVal)]
modifyMap ((k, v) : xs) tId newVal
  | k == tId = (tId, newVal) : xs
  | otherwise = (k, v) : modifyMap xs tId newVal

showDislikeIcon :: Maybe [(Int, Bool)] -> Int -> View CommentCardId ()
showDislikeIcon Nothing _ = tag "i" (cc "bx bx-dislike") none
showDislikeIcon (Just vals) tId = do
  case lookup tId vals of
    Just False -> tag "i" (cc "bx bxs-dislike") none
    _ -> tag "i" (cc "bx bx-dislike") none

showLikeIcon :: Maybe [(Int, Bool)] -> Int -> View CommentCardId ()
showLikeIcon Nothing _ = tag "i" (cc "bx bx-like") none
showLikeIcon (Just vals) tId = do
  case lookup tId vals of
    Just True -> tag "i" (cc "bx bxs-like") none
    _ -> tag "i" (cc "bx bx-like") none

disabledAddCommentButtonView :: View CommentCardId ()
disabledAddCommentButtonView = button
  GoToLogin
  (cc "mt-4 px-4 py-2 rounded bg-green-500 text-white opacity-50 cursor-not-allowed")
  $ do
    "Login to add comment"

addCommentButtonView :: AddCommentData -> View CommentCardId ()
addCommentButtonView addCommentData = button
  (AddCommentBtn addCommentData)
  (cc "mt-4 px-4 py-2 rounded bg-green-500 text-white hover:bg-green-600")
  $ do
    "Add comment"

textarea :: ViewAction (Action id) => (Text -> Action id) -> DelayMs -> Mod id -> View id ()
textarea act delay f =
  tag "textarea" (onInput act delay . f) none

newtype AddCommentForm f = AddCommentForm
  { commentContentField :: Field f Text
  }
  deriving (Generic)

instance Form AddCommentForm Validated

validateForm :: AddCommentForm Identity -> AddCommentForm Validated
validateForm u =
  AddCommentForm
    { commentContentField = validate (T.null $ commentContentField u) "Comment cannot be empty"
    }

addCommentView :: Text -> Int -> Maybe Int -> AddCommentForm Validated -> View CommentCardId ()
addCommentView token tId mParentCommentId v = do
  let f = formFieldsWith v
  let css =
        "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Add comment"

      form @AddCommentForm (SubmitAddComment token tId mParentCommentId) (gap 10) $ do
        field (commentContentField f) (const mempty) $ do
          el (cc "mb-4") $ do
            tag
              "textarea"
              ( cc "w-full px-3 py-2 border rounded"
                  <> att "rows" "4"
                  <> name "commentContentField"
              )
              none
        el (cc "flex justify-end space-x-2") $ do
          submit
            (btn . cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500")
            "Submit"
      el (cc "flex justify-end space-x-2") $ do
        button
          (CancelAddComment tId)
          (cc "mt-2 px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
          "Cancel"

commentCardView :: CommentCardOps -> View CommentCardId ()
commentCardView commentCardOps@CommentCardOps {commentInfo = CommentInfo {..}, ..} = do
  let addCommentData =
        AddCommentData
          { contentForAddComment = ""
          , threadId = threadIDForCommentInfo
          , userToken = fromMaybe "" tokenForCommentCard
          , parentCommentIdForAddComment = Just commentIDForCommentInfo
          }
  el
    ( cc
        "border p-4 mb-2 rounded shadow-sm bg-white card-bg shadow-lg rounded-lg mb-6 overflow-hidden"
    )
    $ do
      el (cc "flex justify-between") $ do
        tag
          "span"
          (cc "font-semibold text-gray-500")
          (text userNameForCommentInfo)
        tag
          "span"
          (cc "font-semibold text-gray-500")
          (text $ T.pack $ show createdAtForCommentInfo)
      tag "p" (cc "mt-2 text-gray-900") (text commentContentForCommentInfo)
      el (cc "flex mt-2 text-sm space-x-2") $ do
        case tokenForCommentCard of
          Nothing -> button GoToLogin (cc "hover:text-blue-500") $ do
            "Login to reply"
          Just _ ->
            button
              (AddCommentBtn addCommentData)
              (cc "hover:text-blue-499 bg-blue-500 rounded-md px-1 py-1")
              "Reply"
        button (LikeComment commentCardOps) (cc "hover:text-green-500") $ do
          showLikeIcon currUserVotes commentIDForCommentInfo
          tag "span" mempty $ text $ T.pack . show $ fromMaybe 0 commentUpvoteCount
        button (DislikeComment commentCardOps) (cc "hover:text-red-500") $ do
          showDislikeIcon currUserVotes commentIDForCommentInfo
          tag "span" mempty $ text $ T.pack . show $ fromMaybe 0 commentDownvoteCount
