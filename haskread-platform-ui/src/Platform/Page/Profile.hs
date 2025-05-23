{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.Profile (profilePage) where

import Data.Base64.Types
import Data.ByteString.Lazy.Base64
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Effectful
import qualified Platform.Common.CSS as CSS
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Platform.View.ThreadCard
import Web.Hyperbole

newtype ProfileId = ProfileId Int
  deriving (Show, Read, ViewId, Generic)

newtype ProfileImageId = ProfileImageId Int
  deriving (Show, Read, ViewId, Generic)

instance (IOE :> es) => HyperView ProfileImageId es where
  data Action ProfileImageId = LoadProfileImage Int
    deriving (Show, Read, ViewAction, Generic)

  update (LoadProfileImage userId) = do
    eImage <- liftIO $ getUserProfileImage userId
    case eImage of
      Right imgBytes -> do
        pure $ el (cc "mb-4") $ do
          tag
            "img"
            ( att "src" (imgData imgBytes)
                . att "alt" "Profile Image"
                . cc "w-32 h-32 rounded-full object-cover"
            )
            none
      Left _ ->
        pure $ tag "p" (cc "text-gray-600 italic") "No profile image available"
    where
      imgData imgBytes = (TL.toStrict $ "data:image/jpeg;base64," <> extractBase64 (encodeBase64 imgBytes))

instance (IOE :> es) => HyperView ProfileId es where
  data Action ProfileId
    = GoToHome
    | ChangePasswordBtn Text
    | CancelChangePassword
    | SubmitChangePassword Text
    | DeleteAccount Text
    | SubmitDeleteAccount Text
    | UpdateImage
    deriving (Show, Read, ViewAction, Generic)

  update UpdateImage = pure updateImageView
  update CancelChangePassword = redirect "/profile"
  update GoToHome = redirect "/"
  update (ChangePasswordBtn token) = pure $ changePasswordView token genFields
  update (DeleteAccount token) = pure $ deleteAccountView token genFields
  update (SubmitChangePassword token) = do
    uf <- formData @(ChangePasswordForm Identity)
    let vals = validateChangePasswordForm uf
    if or
      [ isInvalid $ oldPasswordField vals
      , isInvalid $ newPasswordField vals
      , isInvalid $ confirmNewPasswordField vals
      ]
      then
        pure $ changePasswordView token vals
      else do
        mRes <-
          liftIO $
            changePassword token $
              ChangePasswordBody
                { oldPasswordForChangePass = oldPasswordField uf
                , newPasswordForChangePass = newPasswordField uf
                , confirmPasswordForChangePass = confirmNewPasswordField uf
                }
        case mRes of
          Left e -> liftIO $ putStrLn e
          Right _ -> pure ()
        redirect "/"
  update (SubmitDeleteAccount token) = do
    uf <- formData @(DeleteAccountForm Identity)
    let vals = validateDeleteAccountForm uf
    if or
      [ isInvalid $ deleteAccountPasswordField vals
      , isInvalid $ areYouSureField vals
      ]
      then
        pure $ deleteAccountView token vals
      else do
        mRes <-
          liftIO $
            deleteUser token $
              DeleteUserBody
                { passwordForDeleteUser = deleteAccountPasswordField uf
                , areUSure = areYouSureField uf
                }
        case mRes of
          Left e -> liftIO $ putStrLn e
          Right _ -> deleteSession @AuthData
        redirect "/"

data ChangePasswordForm f = ChangePasswordForm
  { oldPasswordField :: Field f Text
  , newPasswordField :: Field f Text
  , confirmNewPasswordField :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

data DeleteAccountForm f = DeleteAccountForm
  { deleteAccountPasswordField :: Field f Text
  , areYouSureField :: Field f Bool
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

validateDeleteAccountForm :: DeleteAccountForm Identity -> DeleteAccountForm Validated
validateDeleteAccountForm u =
  DeleteAccountForm
    { deleteAccountPasswordField =
        validate (T.null $ deleteAccountPasswordField u) "Field cannot be empty"
    , areYouSureField = validate (not $ areYouSureField u) "Please check this box"
    }

validateChangePasswordForm :: ChangePasswordForm Identity -> ChangePasswordForm Validated
validateChangePasswordForm u =
  ChangePasswordForm
    { oldPasswordField = validate (T.null $ oldPasswordField u) "old password cannot be empty"
    , newPasswordField = validate (T.null $ oldPasswordField u) "old password cannot be empty"
    , confirmNewPasswordField = validate (T.null $ oldPasswordField u) "old password cannot be empty"
    }

profileView :: Text -> View ProfileId ()
profileView token = do
  el (cc CSS.buttonGroupCSS) $ do
    button
      (ChangePasswordBtn token)
      (cc CSS.successButtonCSS)
      "Change Password"
    button
      (DeleteAccount token)
      (cc CSS.dangerButtonCSS)
      "Delete account"
    button
      UpdateImage
      (cc CSS.warningButtonCSS)
      "Update profile image"

changePasswordView ::
  Text ->
  ChangePasswordForm Validated ->
  View ProfileId ()
changePasswordView token _ = do
  let f = fieldNames @ChangePasswordForm
  el (cc CSS.centeredCSS) $ do
    el (cc CSS.cardContainerCSS) $ do
      el (cc CSS.paddedCSS) $ do
        tag "h2" (cc CSS.sectionTitleCSS) $ text "Change Password"

        form (SubmitChangePassword token) (cc CSS.formBaseCSS) $ do
          field (oldPasswordField f) id $ do
            el (cc CSS.formGroupCSS) $ do
              tag "label" (cc CSS.labelCSS) $
                tag "span" (cc CSS.labelTextCSS) "Enter Old Password"
              input TextInput (placeholder "Old Password" . cc CSS.inputFieldCSS)

          field (newPasswordField f) id $ do
            el (cc CSS.formGroupCSS) $ do
              tag "label" (cc CSS.labelCSS) $
                tag "span" (cc CSS.labelTextCSS) "Enter new Password"
              input TextInput (placeholder "new Password" . cc CSS.inputFieldCSS)

          field (confirmNewPasswordField f) id $ do
            el (cc CSS.formGroupCSS) $ do
              tag "label" (cc CSS.labelCSS) $
                tag "span" (cc CSS.labelTextCSS) "Re-enter new Password"
              input TextInput (placeholder "confirm new Password" . cc CSS.inputFieldCSS)

          el (cc CSS.buttonGroupCSS) $ do
            submit (btn . cc CSS.buttonPrimaryCSS) "Submit"

        el (cc CSS.buttonGroupCSS) $ do
          button CancelChangePassword (cc CSS.buttonSecondaryCSS) "Cancel"

deleteAccountView ::
  Text ->
  DeleteAccountForm Validated ->
  View ProfileId ()
deleteAccountView token _ = do
  let f = fieldNames @DeleteAccountForm
  el (cc CSS.centeredCSS) $ do
    el (cc CSS.cardContainerCSS) $ do
      el (cc CSS.paddedCSS) $ do
        tag "h2" (cc CSS.sectionTitleCSS) $ text "Delete account"

        form (SubmitDeleteAccount token) (cc CSS.formBaseCSS) $ do
          field (deleteAccountPasswordField f) id $ do
            el (cc CSS.formGroupCSS) $ do
              tag "label" (cc CSS.labelCSS) $
                tag "span" (cc CSS.labelTextCSS) "Enter Password"
              input TextInput (placeholder "Password" . cc CSS.inputFieldCSS)

          field (areYouSureField f) id $ do
            el (cc CSS.formGroupCSS) $ do
              tag "label" (cc CSS.labelCSS) $
                tag "span" (cc CSS.labelTextCSS) "Are you sure"
              tag
                "input"
                (att "type" "checkbox" . att "value" "true" . att "name" "areYouSureField")
                none

          el (cc CSS.buttonGroupCSS) $ do
            submit (btn . cc CSS.buttonPrimaryCSS) "Delete"

        el (cc CSS.buttonGroupCSS) $ do
          button CancelChangePassword (cc CSS.buttonSecondaryCSS) "Cancel"

updateImageView :: View ProfileId ()
updateImageView = do
  el (cc CSS.centeredCSS) $ do
    el (cc CSS.cardContainerCSS) $ do
      el (cc CSS.paddedCSS) $ do
        tag "h2" (cc CSS.sectionTitleCSS) $ text "Update/upload image"
        el (cc CSS.formGroupCSS) $ do
          tag "label" (cc CSS.labelCSS) $
            tag "span" (cc CSS.labelTextCSS) "Choose image"
          tag
            "input"
            ( att "type" "file"
                . att "id" "imageInput"
                . att "accept" "image/*"
                . att "required" ""
                . att "onChange" "previewImage()"
            )
            none
        el (cc CSS.buttonGroupCSS) $ do
          tag
            "button"
            ( cc CSS.buttonPrimaryCSS
                . att "onClick" "uploadImage()"
            )
            "Upload image"
          button
            CancelChangePassword
            (cc CSS.buttonSecondaryCSS)
            "Cancel"
        el (cc CSS.formGroupCSS) $ do
          tag
            "img"
            ( att "id" "imagePreview"
                . att "style" "display: none; width: 100%; height: auto;"
                . cc CSS.imagePreviewCSS
            )
            none
        tag "p" (att "id" "statusMessage") none

loadingProfileImage :: Int -> View ProfileImageId ()
loadingProfileImage userId =
  el (onLoad (LoadProfileImage userId) 800) $ do
    text "Loading profile image"

profilePage ::
  (Hyperbole :> es, IOE :> es) =>
  Eff
    es
    ( Page
        '[ ProfileId
         , HeaderId
         , ThreadId
         , FooterId
         , LiveSearchId
         , ThreadId
         , AttachmentViewId
         , ProfileImageId
         , LoginProfileBtns
         ]
    )
profilePage = do
  mbUserContext <- genUserContext [] []
  case mbUserContext of
    Nothing -> redirect "/"
    Just uc@UserContext {..} -> do
      mbLimit <- lookupParam "limit"
      mbOffset <- lookupParam "offset"
      mbCommunityId <- lookupParam "communityId"
      eRes <- liftIO $ getAllThreads mbLimit mbOffset mbCommunityId (Just (userIDForUPR ucUserProfile))
      case eRes of
        Left err -> pure $ el_ $ raw (T.pack err)
        Right res -> do
          eUserThreadVotes <- liftIO $ getUserThreadVotes ucToken (getThreadIds res)
          pure $ el (cc CSS.pageContainerCSS) $ do
            stylesheet "style.css"
            script "myjs.js"
            hyper (HeaderId 1) headerView
            tag "main" (cc CSS.mainContainerCSS) $ do
              el (cc CSS.threadListSectionCSS) $ do
                el (cc "w-full px-4") $ do
                  tag "p" (cc CSS.sectionTitleHomeCSS) "Profile"
                  el_ $ do
                    tag "p" (cc CSS.sectionSubtitleCSS) "Welcome to your profile page!"
                    hyper (ProfileImageId 0) (loadingProfileImage (userIDForUPR ucUserProfile))
                    tag "p" (cc CSS.profileInfoCSS) $ text ("Username: " <> userNameForUPR ucUserProfile)
                    hyper (ProfileId 1) $ profileView ucToken
              tag "h1" (cc CSS.sectionHeadingCSS) "Posts by user"
              viewThreadsList (Just $ uc {ucUserThreadVotes = fromRight [] eUserThreadVotes}) (threads res)
            hyper (FooterId 1) footerView
  where
    viewThreadsList mbUserContext threads =
      foldr
        ( \(idx, thread) acc -> do
            (hyper (ThreadId idx) (threadView thread mbUserContext))
            acc
        )
        none
        (zip [0 ..] threads)
