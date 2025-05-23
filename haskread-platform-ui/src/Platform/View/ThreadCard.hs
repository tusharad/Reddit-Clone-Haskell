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

module Platform.View.ThreadCard
  ( ThreadId (..)
  , AttachmentViewId (..)
  , threadView
  , showDislikeIcon
  , showLikeIcon
  , Action (..)
  , update
  )
where

import Control.Monad (forM_)
import Data.Base64.Types
import Data.ByteString.Lazy.Base64
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Effectful
import qualified Platform.Common.CSS as CSS
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import System.FilePath
import Web.Hyperbole

data ThreadId = ThreadId Int
  deriving (Show, Read, Generic, ViewId)

newtype AttachmentViewId = AttachmentViewId Int
  deriving (Show, Read, Generic, ViewId)

instance (IOE :> es) => HyperView AttachmentViewId es where
  data Action AttachmentViewId = LoadImage Int
    deriving (Show, Read, Generic, ViewAction)

  update (LoadImage threadId) = do
    eRes <- liftIO $ getAttachment threadId
    case eRes of
      Left _ -> pure $ el_ "Failed to load attachment"
      Right docContent ->
        pure $
          el (cc "p-4") $
            tag
              "img"
              ( att "src" (TL.toStrict $ "data:image/jpeg;base64," <> extractBase64 (encodeBase64 docContent))
                  . att "alt" "error...image supposed to be here"
                  . cc "w-full h-auto rounded-md"
              )
              none

instance (IOE :> es) => HyperView ThreadId es where
  data Action ThreadId
    = UpdateUpVote ThreadInfo (Maybe UserContext)
    | UpdateDownVote ThreadInfo (Maybe UserContext)
    | DeleteThread ThreadInfo UserContext
    | EditThread ThreadInfo UserContext
    | CancelEditThreadForm ThreadInfo (Maybe UserContext)
    deriving (Show, Read, ViewAction, Generic)

  type Require ThreadId = '[AttachmentViewId]

  update (UpdateUpVote threadInfo@ThreadInfo {..} mbUserContext) = do
    case mbUserContext of
      Nothing -> pure $ threadView threadInfo mbUserContext
      Just uc@UserContext {..} -> do
        eRes <- liftIO $ voteThread "upvote" ucToken threadIDForThreadInfo
        case eRes of
          Left err -> do
            liftIO $ putStrLn err
            pure $ threadView threadInfo mbUserContext
          Right _ -> do
            let (updatedVotes, updatedThreadInfo) = applyVote ucUserThreadVotes threadInfo threadIDForThreadInfo True
            pure $ threadView updatedThreadInfo (Just uc {ucUserThreadVotes = updatedVotes})
  update (UpdateDownVote threadInfo@ThreadInfo {..} mbUserContext) = do
    case mbUserContext of
      Nothing -> pure $ threadView threadInfo mbUserContext
      Just uc@UserContext {..} -> do
        eRes <- liftIO $ voteThread "downvote" ucToken threadIDForThreadInfo
        case eRes of
          Left err -> do
            liftIO $ putStrLn err
            pure $ threadView threadInfo mbUserContext
          Right _ -> do
            let (updatedVotes, updatedThreadInfo) = applyVote ucUserThreadVotes threadInfo threadIDForThreadInfo False
            pure $ threadView updatedThreadInfo (Just uc {ucUserThreadVotes = updatedVotes})
  update (DeleteThread threadInfo@ThreadInfo {..} uc@UserContext {..}) = do
    if userIDForThreadInfo == (userIDForUPR ucUserProfile)
      then do
        _ <- liftIO $ deleteThread threadIDForThreadInfo ucToken
        redirect "/"
      else do
        pure $ threadView threadInfo (Just uc)
  update (CancelEditThreadForm _ _) = redirect "/"
  update (EditThread threadInfo uc) = do
    eCommunityList <- liftIO getCommunityList
    case eCommunityList of
      Left err -> do
        liftIO $ putStrLn $ "Error: " <> err
        pure $ threadView threadInfo (Just uc)
      Right communityList -> pure $ editThreadView threadInfo communityList

applyVote :: [(Int, Bool)] -> ThreadInfo -> Int -> Bool -> ([(Int, Bool)], ThreadInfo)
applyVote votes threadInfo tId newVote =
  let oldVote = lookup tId votes
      (updatedVotes, upChange, downChange) = case oldVote of
        Nothing ->
          ((tId, newVote) : votes, if newVote then 1 else 0, if newVote then 0 else 1)
        Just prevVote
          | prevVote == newVote ->
              (filter ((/= tId) . fst) votes, if newVote then -1 else 0, if newVote then 0 else -1)
          | otherwise ->
              ( map (\(k, v) -> if k == tId then (k, newVote) else (k, v)) votes
              , if newVote then 1 else -1
              , if newVote then -1 else 1
              )

      newUpvotes = Just $ fromMaybe 0 (upvoteCount threadInfo) + upChange
      newDownvotes = Just $ fromMaybe 0 (downvoteCount threadInfo) + downChange
      newThreadInfo = threadInfo {upvoteCount = newUpvotes, downvoteCount = newDownvotes}
   in (updatedVotes, newThreadInfo)

showLikeIcon :: Maybe UserContext -> Int -> View ThreadId ()
showLikeIcon = showVoteIcon True

showDislikeIcon :: Maybe UserContext -> Int -> View ThreadId ()
showDislikeIcon = showVoteIcon False

showVoteIcon :: Bool -> Maybe UserContext -> Int -> View ThreadId ()
showVoteIcon isLike mbUserContext tId = do
  let (solidClass, outlinedClass) =
        if isLike
          then ("bx bxs-like text-gray-600 dark:text-gray-300", "bx bx-like text-gray-600 dark:text-gray-300")
          else
            ( "bx bxs-dislike text-gray-600 dark:text-gray-300"
            , "bx bx-dislike text-gray-600 dark:text-gray-300"
            )

      vote = mbUserContext >>= \UserContext {ucUserThreadVotes = votes} -> lookup tId votes
      iconClass = case vote of
        Just v | v == isLike -> solidClass
        _ -> outlinedClass

  tag "i" (cc iconClass) none

attachmentView :: Text -> Int -> View AttachmentViewId ()
attachmentView attachmentName threadId
  | isImage = el (cc "mb-4" . onLoad (LoadImage threadId) 500) $ text "Loading attachment..."
  | otherwise = el (cc "mb-4") $ do
      let funcName = mconcat ["downloadAttachment(", T.pack (show threadId), ",'", attachmentName, "')"]
      tag
        "button"
        (cc "text-blue-500" . att "onClick" funcName)
        (text attachmentName)
  where
    isImage =
      takeExtension
        (T.unpack attachmentName)
        `elem` [".png", ".jpeg", ".jpg", ".svg", ".gif"]

threadView :: ThreadInfo -> Maybe UserContext -> View ThreadId ()
threadView threadInfo@ThreadInfo {..} mbUserContext = do
  el (cc CSS.cardContainerCSS) $ do
    el (cc CSS.threadHeaderCSS) $ do
      tag "h2" (cc CSS.threadTitleCSS) $ do
        link
          (stringToUrl $ "/view-thread/" <> show threadIDForThreadInfo)
          (cc CSS.threadTitleLinkCSS)
          (text title)

      el (cc CSS.threadMetaCSS) $ do
        tag "p" mempty $ text "Community:"
        tag "span" (cc "font-semibold") (text communityNameForThreadInfo)
        tag "p" mempty $ do
          text "Created by:"
          tag "span" (cc "font-semibold") (text userNameForThreadInfo)
        tag "p" mempty (text createdAtForThreadInfo)

    el (cc CSS.threadDescriptionCSS) $ do
      tag "p" mempty (text $ fromMaybe "" description)
      case attachmentName of
        Just attName ->
          hyper (AttachmentViewId threadIDForThreadInfo) $
            attachmentView attName threadIDForThreadInfo
        Nothing -> none

    el (cc CSS.threadActionsCSS) $ do
      el (cc "flex space-x-2 items-center") $ do
        button (UpdateUpVote threadInfo mbUserContext) (cc CSS.threadActionButtonCSS) $ do
          showLikeIcon mbUserContext threadIDForThreadInfo
          tag "span" (cc CSS.threadActionTextCSS) (text $ toText (fromMaybe 0 upvoteCount))

        button (UpdateDownVote threadInfo mbUserContext) (cc CSS.threadActionButtonCSS) $ do
          showDislikeIcon mbUserContext threadIDForThreadInfo
          tag "span" (cc CSS.threadActionTextCSS) (text $ toText (fromMaybe 0 downvoteCount))

        renderEditDeleteButtons mbUserContext threadInfo
        tag "span" (cc "flex items-center space-x-1") $ do
          tag "i" (cc CSS.commentCountIconCSS) none
          tag "span" (cc CSS.threadActionTextCSS) (text $ toText (fromMaybe 0 commentCount))

editThreadView :: ThreadInfo -> Communities -> View ThreadId ()
editThreadView ThreadInfo {..} (Communities communityList) = do
  let css = CSS.centeredCSS
      funcName = "updateThread(" <> show threadIDForThreadInfo <> ")"
  el (cc css) $ do
    el (cc CSS.modalCardCSS) $ do
      tag "h2" (cc CSS.sectionTitleCSS) $ text "Edit Thread"
      tag "div" (gap 10) $ do
        el (cc CSS.formGroupCSS) $ do
          tag "span" (att "id" "statusMessage" . cc "text-green-500") none
        el (cc CSS.formGroupCSS) $ do
          tag "label" (cc CSS.labelCSS) "Select community"
          tag "select" (cc CSS.selectCSS . att "id" "threadCommunityID") $ do
            forM_ communityList $ \c -> do
              tag
                "option"
                (att "value" (toText $ communityID c))
                (raw $ communityName c)
        el (cc CSS.formGroupCSS) $ do
          tag "label" (cc CSS.labelCSS) "Enter title"
          tag
            "input"
            ( att "type" "text"
                . placeholder "Title"
                . cc CSS.inputCSS
                . att "id" "threadTitle"
                . att "value" title
            )
            none
        el (cc CSS.formGroupCSS) $ do
          tag "label" (cc CSS.labelCSS) "Enter Description"
          tag
            "textarea"
            (att "id" "threadDescription" . maybe mempty (att "value") description . cc CSS.inputCSS)
            none
        el (cc CSS.formGroupCSS) $ do
          tag "label" (cc CSS.disabledLabelCSS) $
            "Cannot update file, if want to update, please delete post"
        el (cc CSS.formGroupCSS) $ do
          tag "button" (att "onClick" (T.pack funcName) . cc CSS.buttonCSS) "Update"
          tag "button" (att "onClick" "cancelForm()" . cc CSS.buttonCSS) "Cancel"

renderEditDeleteButtons ::
  Maybe UserContext ->
  ThreadInfo ->
  View ThreadId ()
renderEditDeleteButtons mbUserContext threadInfo =
  case mbUserContext of
    Just (uc@(UserContext _ userInfo _ _)) ->
      if userIDForUPR userInfo == userIDForThreadInfo threadInfo -- If user is same as thread author
        then do
          button (DeleteThread threadInfo uc) (cc CSS.threadActionButtonCSS) "delete"
          button (EditThread threadInfo uc) (cc CSS.threadActionButtonCSS) "edit"
        else none
    Nothing -> none
