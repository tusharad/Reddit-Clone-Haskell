{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.Page.Profile (profilePage) where

import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Effectful
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Platform.View
import Platform.View.Header
import Platform.View.LiveSearch (LiveSearchId)
import Platform.View.ThreadCard
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData
import Data.ByteString.Lazy.Base64

newtype ProfileId = ProfileId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView ProfileId es where
  data Action ProfileId
    = GoToHome
    | ChangePasswordBtn Text
    | CancelChangePassword
    | SubmitChangePassword Text
    | DeleteAccount Text
    | SubmitDeleteAccount Text
    | UpdateImage
    deriving (Show, Read, ViewAction)

  update UpdateImage = pure updateImageView
  update CancelChangePassword = redirect "/profile"
  update GoToHome = redirect "/"
  update (ChangePasswordBtn token) = pure $ changePasswordView token genForm
  update (DeleteAccount token) = pure $ deleteAccountView token genForm
  update (SubmitChangePassword token) = do
    uf <- formData @ChangePasswordForm
    let vals = validateChangePasswordForm uf
    if anyInvalid vals
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
    uf <- formData @DeleteAccountForm
    let vals = validateDeleteAccountForm uf
    if anyInvalid vals
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
  deriving (Generic)

instance Form ChangePasswordForm Validated

data DeleteAccountForm f = DeleteAccountForm
  { deleteAccountPasswordField :: Field f Text
  , areYouSureField :: Field f Bool
  }
  deriving (Generic)

instance Form DeleteAccountForm Validated

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
  el (cc "flex justify-end space-x-2") $ do
    button
      (ChangePasswordBtn token)
      (cc "px-4 py-2 bg-green-600 text-white rounded hover:bg-green-200")
      "Change Password"
    button
      (DeleteAccount token)
      (cc "px-4 py-2 bg-red-600 text-white rounded hover:bg-red-200")
      "Delete account"
    button
      UpdateImage
      (cc "px-4 py-2 bg-yellow-600 text-white rounded hover:bg-red-200")
      "Update profile image"

changePasswordView ::
  Text ->
  ChangePasswordForm Validated ->
  View ProfileId ()
changePasswordView token v = do
  let f = formFieldsWith v
  let css =
        "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
      inputFieldCSS = "w-full px-3 py-2 border rounded"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Change Password"

      form @ChangePasswordForm (SubmitChangePassword token) (gap 10) $ do
        field (oldPasswordField f) valStyle $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Enter Old Password"
            input TextInput (placeholder "Old Password" . cc inputFieldCSS)
            el_ invalidText

        field (newPasswordField f) valStyle $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Enter new Password"
            input TextInput (placeholder "new Password" . cc inputFieldCSS)
            el_ invalidText

        field (confirmNewPasswordField f) valStyle $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Re-enter new Password"
            input TextInput (placeholder "confirm new Password" . cc inputFieldCSS)
            el_ invalidText

        el (cc "flex justify-end space-x-2") $ do
          submit
            (btn . cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500")
            "Submit"

      el (cc "flex justify-end space-x-2") $ do
        button
          CancelChangePassword
          (cc "mt-2 px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
          "Cancel"
  where
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id

deleteAccountView :: Text -> DeleteAccountForm Validated -> View ProfileId ()
deleteAccountView token v = do
  let f = formFieldsWith v
  let css =
        "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
      inputFieldCSS = "w-full px-3 py-2 border rounded"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Delete account"

      form @DeleteAccountForm (SubmitDeleteAccount token) (gap 10) $ do
        field (deleteAccountPasswordField f) valStyle $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Enter Password"
            input TextInput (placeholder "Password" . cc inputFieldCSS)
            el_ invalidText

        field (areYouSureField f) valStyle $ do
          el (cc "mb-4") $ do
            tag "label" (cc "block text-gray-700") "Are you sure"
            tag
              "input"
              (att "type" "checkbox" . att "value" "true" . att "name" "areYouSureField")
              none
            el_ invalidText

        el (cc "flex justify-end space-x-2") $ do
          submit
            (btn . cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500")
            "Delete"

      el (cc "flex justify-end space-x-2") $ do
        button
          CancelChangePassword
          (cc "mt-2 px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
          "Cancel"
  where
    valStyle (Invalid _) = invalid
    valStyle Valid = success
    valStyle _ = id

updateImageView :: View ProfileId ()
updateImageView = do
  let css =
        "fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center"
  el (cc css) $ do
    el (cc "bg-white p-8 rounded-lg shadow-lg max-w-md w-full") $ do
      tag "h2" (cc "text-2xl font-bold mb-4") $ text "Update/upload image"
      el (cc "mb-4") $ do
          tag "label" (cc "block text-gray-700") "Choose image"
          tag "input" (att "type" "file"
                     . att "id" "imageInput"
                     . att "accept" "image/*"
                     . att "required" ""
                     . att "onChange" "previewImage()"
                     )
                     none
      el (cc "flex justify-end space-x-2") $ do
        tag "button" (cc "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500"
            . att "onClick" "uploadImage()")
            "Upload image"
        button
          CancelChangePassword
          (cc "px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500")
          "Cancel"
      tag "img" (att "id" "imagePreview" . att "style" "display: none; max-width: 100%; height: auto;") none
      tag "p" (att "id" "statusMessage") none

profilePage ::
  (Hyperbole :> es, IOE :> es) =>
  Eff es (Page '[ProfileId, HeaderId, ThreadId, FooterId, LiveSearchId])
profilePage = do
  mbTokenAndUser <- getTokenAndUser
  case mbTokenAndUser of
    Nothing -> redirect "/"
    Just (token_, userInfo) -> do
      mbLimit <- lookupParam $ Param "limit"
      mbOffset <- lookupParam $ Param "offset"
      mbCommunityId <- lookupParam $ Param "communityId"
      eRes <-
        liftIO
          ( getAllThreads
              mbLimit
              mbOffset
              mbCommunityId
              (Just $ userIDForUPR userInfo)
          )
      eImage <- liftIO $ getUserProfileImage (userIDForUPR userInfo)
      case eRes of
        Left err -> pure $ el_ $ raw (T.pack err)
        Right res -> do
          eUserThreadVotes <- liftIO $ getUserThreadVotes token_ (getThreadIds res)
          pure $ col (pad 20) $ do
            stylesheet "style.css"
            script "myjs.js"
            hyper (HeaderId 1) (headerView $ HeaderOps (Just token_) (Just userInfo))
            tag "main" (cc "container mx-auto mt-16 px-6 flex-grow") $ do
              el (cc "flex flex-col min-h-screen bg-[#F4EEFF]") $ do
                el (cc "mb-6") $ do
                  tag "h1" (cc "text-3xl font-bold text-center mb-4") "Profile"
                  el (cc "bg-white shadow-lg rounded-lg p-6") $ do
                    tag "p" (cc "text-lg text-gray-800") "Welcome to your profile page!"
                    case eImage of
                      Right imgBytes -> do
                        el (cc "mb-4") $ do
                          tag "img" 
                            ( att "src" (TL.toStrict $ "data:image/jpeg;base64," <> encodeBase64 imgBytes)
                            . att "alt" "Profile Image"
                            . cc "w-32 h-32 rounded-full object-cover"
                            ) 
                            none
                      Left _ -> 
                        tag "p" (cc "text-gray-600 italic") "No profile image available"
                    tag
                      "p"
                      (cc "text-bold")
                      (text $ "Username:" `append` userNameForUPR userInfo)

                    hyper (ProfileId 1) $ profileView token_
                  tag "h1" (cc "text-3xl font-bold text-center mb-4") "Posts by users"
                  viewThreadsList
                    (Just userInfo)
                    (Just token_)
                    (hush eUserThreadVotes)
                    0
                    (threads res)
              hyper (FooterId 1) footerView
