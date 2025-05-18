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
import qualified Platform.Common.CSS as CSS

data CommentCardOps = CommentCardOps
  { currUserVotes :: Maybe [(Int, Bool)]
  , tokenForCommentCard :: Maybe Text
  , commentInfo :: CommentInfo
  , mbUserInfoForCommentCard :: Maybe UserProfileResponse
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

newtype CommentCardId = CommentCardId Int
  deriving (Show, Read, ViewId, Generic)

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
    deriving (Show, Read, ViewAction, Generic)

  update (SubmitEditComment cId token tId) = do
    uf <- formData @(EditCommentForm Identity)
    _ <- liftIO $ editComment cId token (commentContentForEdit uf)
    redirect . url $ T.pack ("/view-thread/" <> show tId)
  update GoToLogin = redirect "/login"
  update (AddCommentBtn AddCommentData {..}) =
    pure $ addCommentView userToken threadId parentCommentIdForAddComment genFields
  update (DoRedirect tId) = redirect . url $ T.pack ("/view-thread/" <> show tId)
  update (CancelAddComment threadId) =
    redirect . url $ T.pack ("/view-thread/" <> show threadId)
  update (SubmitAddComment token tId mParentCommentId) = do
    uf <- formData @(AddCommentForm Identity)
    let vals = validateForm uf
    if or [isInvalid $ commentContentField vals]
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
  button GoToLogin (cc $ CSS.primaryButtonCSS <> " " <> CSS.disabledButtonCSS) $
    "Login to add comment"
    
addCommentButtonView :: AddCommentData -> View CommentCardId ()
addCommentButtonView addCommentData =
  button (AddCommentBtn addCommentData) (cc CSS.primaryButtonCSS) $
    "Add comment"

newtype AddCommentForm f = AddCommentForm
  { commentContentField :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

validateForm :: AddCommentForm Identity -> AddCommentForm Validated
validateForm u =
  AddCommentForm
    { commentContentField = validate (T.null $ commentContentField u) "Comment cannot be empty"
    }

newtype EditCommentForm f = EditCommentForm
  { commentContentForEdit :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

helperCommentView ::
  Text ->
  (View (FormFields id) () -> View CommentCardId a) ->
  View (FormFields id) b ->
  Int ->
  View CommentCardId ()
helperCommentView cardTitle formTag inpField a = do
  el (cc CSS.centeredCSS) $ do
    el (cc CSS.cardContainerCSS) $ do
      el (cc CSS.paddedCSS) $ do
        tag "h2" (cc CSS.sectionTitleCSS) (text cardTitle)
        void $ formTag $ do
          void inpField
          el (cc CSS.buttonGroupCSS) $ do
            submit
              (headerButtonCSS CSS.primaryButtonCSS)
              "Submit"
        el (cc CSS.buttonGroupCSS) $ do
          button (CancelAddComment a) (headerButtonCSS CSS.secondaryButtonCSS) "Cancel"

editCommentView ::
  Int ->
  Text ->
  Int ->
  EditCommentForm Maybe ->
  View CommentCardId ()
editCommentView commentId token tId v = do
  let f = fieldNames @EditCommentForm
  helperCommentView
    "Edit Comment"
    (form (SubmitEditComment commentId token tId) (gap 10))
    ( field (commentContentForEdit f) (const mempty) $ do
        el (cc CSS.formGroupCSS) $ do
          textarea
            ( cc CSS.textareaCSS
                . att "maxlength" "250"
                . att "oninput" "updateCharCount(this)"
            )
            (commentContentForEdit v)
          el (cc CSS.charCountLabelCSS . att "id" "charCountDiv") $ do
            tag "span" (att "id" "charCount") "250"
            " characters remaining"
    )
    tId

addCommentView ::
  Text ->
  Int ->
  Maybe Int ->
  AddCommentForm Validated ->
  View CommentCardId ()
addCommentView token tId mParentCommentId _ = do
  let f = fieldNames @AddCommentForm
  helperCommentView
    "Add Comment"
    (form (SubmitAddComment token tId mParentCommentId) (gap 10))
    ( field (commentContentField f) (const mempty) $ do
        el (cc CSS.formGroupCSS) $ do
          textarea
            ( cc CSS.textareaCSS
                . att "maxlength" "250"
                . att "oninput" "updateCharCount(this)"
            )
            Nothing
          el (cc CSS.charCountLabelCSS . att "id" "charCountDiv") $ do
            tag "span" (att "id" "charCount") "250"
            " characters remaining"
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

  el (cc CSS.whiteBackgroundShadowCSS) $ do
    el (cc CSS.p4CSS) $ do
      el (cc $ CSS.flexBetweenItemsStartCSS <> " " <> CSS.borderBottomCSS) $ do
        tag "span" (cc CSS.fontBoldCSS) (text userNameForCommentInfo)
        tag "span" (cc CSS.fontBoldCSS) $
          text $ T.pack $ show createdAtForCommentInfo

    el (cc $ CSS.mt2CSS <> " " <> CSS.p4CSS) $
      tag "p" mempty (text commentContentForCommentInfo)

    el (cc CSS.commentVotesCSS) $ do
      case tokenForCommentCard of
        Nothing ->
          button GoToLogin (cc CSS.buttonBaseCSS) "Login to reply"
        Just _ ->
          button (AddCommentBtn addCommentData) (cc CSS.buttonBaseCSS) "Reply"

      button (LikeComment commentCardOps) (cc CSS.buttonBaseCSS) $ do
        showLikeIcon currUserVotes commentIDForCommentInfo
        tag "span" mempty $ text $ T.pack . show $ fromMaybe 0 commentUpvoteCount

      button (DislikeComment commentCardOps) (cc CSS.buttonBaseCSS) $ do
        showDislikeIcon currUserVotes commentIDForCommentInfo
        tag "span" mempty $ text $ T.pack . show $ fromMaybe 0 commentDownvoteCount

      case mbUserInfoForCommentCard of
        Nothing -> none
        Just userInfo ->
          if userIDForUPR userInfo == userIDForCommentInfo
            then do
              button (DeleteComment commentCardOps) (cc CSS.buttonBaseCSS) "delete"
              button (EditComment commentCardOps) (cc "text-sm flex hover:bg-gray-900 bg-gray-700 rounded-md px-1 py-1") "edit"
            else none
