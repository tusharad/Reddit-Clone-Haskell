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
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text, append)
import qualified Data.Text as T
import Effectful (IOE)
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View.Header (headerButtonCSS)
import Web.Hyperbole hiding (input)

data CommentCardOps = CommentCardOps
  { currUserVotes :: Maybe [(Int, Bool)]
  , tokenForCommentCard :: Maybe Text
  , commentInfo :: CommentInfo
  , mbUserInfoForCommentCard :: Maybe UserProfileResponse
  }
  deriving (Show, Eq, Read)

newtype CommentCardId = CommentCardId Int
  deriving (Show, Read, ViewId)

instance (IOE :> es) => HyperView CommentCardId es where
  data Action CommentCardId
    = LikeComment CommentCardOps
    | DislikeComment CommentCardOps
    | DeleteComment CommentCardOps
    | EditComment CommentCardOps
    | AddCommentBtn AddCommentData
    | SubmitAddComment Text Int (Maybe Int)
    | SubmitEditComment Int Text Int
    | CancelAddComment Int
    | DoRedirect Int
    | GoToLogin
    deriving (Show, Read, ViewAction)

  update (SubmitEditComment cId token tId) = do
    uf <- formData @EditCommentForm
    _ <- liftIO $ editComment cId token (commentContentForEdit uf)
    redirect . url $ T.pack ("/view-thread/" <> show tId)
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
    void $ liftIO $ upvoteComment tokenForCommentCard commentId
    pure $
      commentCardView
        commentCardOps
          { currUserVotes = updateCurrUserVotes currUserVotes commentId True
          , commentInfo = updateVoteCount currUserVotes True commentInfo
          }
  update (DislikeComment commentCardOps@CommentCardOps {..}) = do
    let commentId = commentIDForCommentInfo commentInfo
    void $ liftIO $ downvoteComment tokenForCommentCard commentId
    pure $
      commentCardView
        commentCardOps
          { currUserVotes = updateCurrUserVotes currUserVotes commentId False
          , commentInfo = updateVoteCount currUserVotes False commentInfo
          }
  update (DeleteComment CommentCardOps {..}) = do
    let commentId = commentIDForCommentInfo commentInfo
        tId = threadIDForCommentInfo commentInfo
    case tokenForCommentCard of
      Nothing -> redirect $ url $ "/view-thread/" `append` toText tId
      Just token -> do
        _ <- liftIO $ deleteComment commentId token
        redirect $ url $ "/view-thread/" `append` toText tId
  update (EditComment CommentCardOps {..}) = do
    let commentId = commentIDForCommentInfo commentInfo
    case tokenForCommentCard of
      Nothing -> redirect "/"
      Just token -> do
        pure $
          editCommentView
            commentId
            token
            (threadIDForCommentInfo commentInfo)
            (EditCommentForm {commentContentForEdit = Just $ commentContentForCommentInfo commentInfo})

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
disabledAddCommentButtonView =
  button
    GoToLogin
    ( cc
        "px-4 py-2 text-white rounded-full font-semibold bg-blue-600 hover:bg-blue-700 transition transform hover:scale-105 opacity-50 cursor-not-allowed"
    )
    $ "Login to add comment"

addCommentButtonView :: AddCommentData -> View CommentCardId ()
addCommentButtonView addCommentData =
  button
    (AddCommentBtn addCommentData)
    ( cc
        "px-4 py-2 text-white rounded-full font-semibold bg-blue-600 hover:bg-blue-700 transition transform hover:scale-105"
    )
    "Add comment"

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

newtype EditCommentForm f = EditCommentForm
  { commentContentForEdit :: Field f Text
  }
  deriving (Generic)

instance Form EditCommentForm Maybe

helperCommentView cardTitle formTag inpField a = do
  let css =
        "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el
      ( cc
          "bg-white dark:bg-gray-700 shadow-lg rounded-lg mb-6 overflow-hidden hover:shadow-xl transition-shadow duration-300"
      )
      $ do
        el (cc "p-6") $ do
          tag "h2" (cc "text-2xl font-bold mb-4 dark:bg-gray-700 dark:border-gray-600 dark:text-white") $
            text cardTitle
          void $ formTag $ do
            void inpField
            el (cc "flex justify-end space-x-2") $ do
              submit
                (headerButtonCSS "bg-blue-600 hover:bg-blue-500 transition transform hover:scale-105")
                "Submit"
          el (cc "flex justify-end space-x-2") $ do
            button
              (CancelAddComment a)
              (headerButtonCSS "mt-2 bg-blue-600 hover:bg-blue-500 transition transform hover:scale-105")
              "Cancel"

editCommentView ::
  Int ->
  Text ->
  Int ->
  EditCommentForm Maybe ->
  View CommentCardId ()
editCommentView commentId token tId v = do
  let f = formFieldsWith v
  helperCommentView
    "Edit Comment"
    (form @EditCommentForm (SubmitEditComment commentId token tId) (gap 10))
    ( field (commentContentForEdit f) (const mempty) $ do
        el (cc "mb-4") $ do
          textarea
            ( cc "w-full px-3 py-2 border rounded"
                . att "maxlength" "250"
                . att "oninput" "updateCharCount(this)"
            )
            (commentContentForEdit v)
          el (cc "text-right text-sm text-green-600" . att "id" "charCountDiv") $ do
            tag "span" (att "id" "charCount") "250"
            "characters remaining"
    )
    tId

addCommentView ::
  Text ->
  Int ->
  Maybe Int ->
  AddCommentForm Validated ->
  View CommentCardId ()
addCommentView token tId mParentCommentId v = do
  let f = formFieldsWith v
  helperCommentView
    "Add Comment"
    (form @EditCommentForm (SubmitAddComment token tId mParentCommentId) (gap 10))
    ( field (commentContentField f) (const mempty) $ do
        el (cc "mb-4") $ do
          textarea
            ( cc "w-full px-3 py-2 border rounded"
                . att "maxlength" "250"
                . att "oninput" "updateCharCount(this)"
            )
            Nothing
          el (cc "text-right text-sm text-green-600" . att "id" "charCountDiv") $ do
            tag "span" (att "id" "charCount") "250"
            "characters remaining"
    )
    tId

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
        "comment bg-white dark:bg-gray-800 shadow-lg rounded-lg mb-6 overflow-hidden hover:shadow-xl transition-shadow duration-300"
    )
    $ do
      el (cc "comment-content p-4") $ do
        el
          ( cc
              "flex flex-col sm:flex-row justify-between items-start sm:items-center border-b dark:border-gray-700"
          )
          $ do
            tag
              "span"
              (cc "font-semibold")
              (text userNameForCommentInfo)
            tag
              "span"
              (cc "font-semibold")
              (text $ T.pack $ show createdAtForCommentInfo)
      el (cc "mt-2 p-4") $
        tag "p" mempty (text commentContentForCommentInfo)
      el (cc "flex mt-2 space-x-2 p-2") $ do
        case tokenForCommentCard of
          Nothing ->
            button
              GoToLogin
              ( cc
                  "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
              )
              $ "Login to reply"
          Just _ ->
            button
              (AddCommentBtn addCommentData)
              ( cc
                  "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
              )
              "Reply"
        button
          (LikeComment commentCardOps)
          ( cc
              "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
          )
          $ do
            showLikeIcon currUserVotes commentIDForCommentInfo
            tag "span" mempty $ text $ T.pack . show $ fromMaybe 0 commentUpvoteCount
        button
          (DislikeComment commentCardOps)
          ( cc
              "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
          )
          $ do
            showDislikeIcon currUserVotes commentIDForCommentInfo
            tag "span" mempty $ text $ T.pack . show $ fromMaybe 0 commentDownvoteCount
        case mbUserInfoForCommentCard of
          Nothing -> none
          Just userInfo -> do
            if userIDForUPR userInfo == userIDForCommentInfo
              then do
                button
                  (DeleteComment commentCardOps)
                  ( cc
                      "flex items-center space-x-1 p-2 rounded-full hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105"
                  )
                  "delete"
                button
                  (EditComment commentCardOps)
                  (cc "text-sm flex hover:bg-gray-900 bg-gray-700 rounded-md px-1 py-1")
                  "edit"
              else none
