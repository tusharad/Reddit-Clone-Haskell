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
    adminMarshaller,
    adminIDField,
    communityMarshaller,
    communityIDField,
    communityNameField
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Orville.PostgreSQL
import Platform.DB.Model

adminNameField :: FieldDefinition NotNull Text
adminNameField = boundedTextField "admin_name" 255

adminIDField :: FieldDefinition NotNull AdminID
adminIDField = coerceField $ serialField "admin_id"

userIDField :: FieldDefinition NotNull UserID
userIDField = coerceField $ serialField "user_id"

userNameField :: FieldDefinition NotNull Text
userNameField = boundedTextField "user_name" 255

emailField :: FieldDefinition NotNull Text
emailField = boundedTextField "email" 255

passwordField :: FieldDefinition NotNull Text
passwordField = unboundedTextField "password"

communityIDField :: FieldDefinition NotNull CommunityID
communityIDField = coerceField $ serialField "community_id"

communityNameField :: FieldDefinition NotNull Text
communityNameField = boundedTextField "community_name" 255

communityDescriptionField :: FieldDefinition NotNull Text
communityDescriptionField = unboundedTextField "community_description"

communityLabelListField :: FieldDefinition NotNull Text
communityLabelListField = unboundedTextField "label_list"

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

adminMarshaller ::
  SqlMarshaller
    AdminWrite
    AdminRead
adminMarshaller =
  Admin
    <$> marshallReadOnly
      ( marshallField
          (\Admin {..} -> adminID)
          adminIDField
      )
    <*> marshallField (\Admin {..} -> adminEmail) emailField
    <*> marshallField (\Admin {..} -> adminName) adminNameField
    <*> marshallField (\Admin {..} -> adminPassword) passwordField
    <*> marshallReadOnly (marshallField (\Admin {..} -> createdAtForAdmin) createdAtField)
    <*> marshallReadOnly (marshallField (\Admin {..} -> updatedAtForAdmin) updatedAtField)

communityMarshaller ::
  SqlMarshaller
    CommunityWrite
    CommunityRead
communityMarshaller =
  Community
    <$> marshallReadOnly
      ( marshallField
          (\Community {..} -> communityID)
          communityIDField
      )
    <*> marshallField (\Community {..} -> communityName) communityNameField
    <*> marshallField (\Community {..} -> communityDescription) communityDescriptionField
    <*> marshallField (\Community {..} -> communityLabelList) communityLabelListField
    <*> marshallReadOnly (marshallField (\Community {..} -> communityCreatedAt) createdAtField)
    <*> marshallReadOnly (marshallField (\Community {..} -> communityUpdatedAt) updatedAtField)
  

