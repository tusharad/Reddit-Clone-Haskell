{-# LANGUAGE RecordWildCards #-}

module Platform.DB.Marshaller
  ( userIDField,
    userNameField,
    emailField,
    passwordField,
    createdAtField,
    updatedAtField,
    userMarshaller,
    userProfileImageMarshaller,
    userImageField,
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Orville.PostgreSQL
import Platform.DB.Model

userIDField :: FieldDefinition NotNull UserID
userIDField = coerceField $ serialField "user_id"

userNameField :: FieldDefinition NotNull Text
userNameField = boundedTextField "user_name" 255

emailField :: FieldDefinition NotNull Text
emailField = boundedTextField "email" 255

passwordField :: FieldDefinition NotNull Text
passwordField = unboundedTextField "password"

createdAtField :: FieldDefinition NotNull UTCTime
createdAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "created_at"

updatedAtField :: FieldDefinition NotNull UTCTime
updatedAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "updated_at"

userImageField :: FieldDefinition NotNull Text
userImageField = unboundedTextField "user_profile_image"

userMarshaller :: SqlMarshaller UserWrite UserRead
userMarshaller =
  User
    <$> marshallReadOnly
      ( marshallField
          (\User {..} -> userID)
          userIDField
      )
    <*> marshallField (\User {..} -> userName) userNameField
    <*> marshallField (\User {..} -> email) emailField
    <*> marshallField (\User {..} -> password) passwordField
    <*> marshallReadOnly
      ( marshallField
          (\User {..} -> createdAt)
          createdAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\User {..} -> updatedAt)
          updatedAtField
      )

userProfileImageMarshaller ::
  SqlMarshaller
    UserProfileImageWrite
    UserProfileImageRead
userProfileImageMarshaller =
  UserProfileImage
    <$> marshallField
      (\UserProfileImage {..} -> userIDForProfileImage)
      userIDField
    <*> marshallField (\UserProfileImage {..} -> userImage) userImageField
    <*> marshallReadOnly
      ( marshallField
          (\UserProfileImage {..} -> createdAtForProfileImage)
          createdAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\UserProfileImage {..} -> updatedAtForProfileImage)
          updatedAtField
      )
