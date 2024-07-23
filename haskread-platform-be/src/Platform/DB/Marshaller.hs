{-# LANGUAGE RecordWildCards #-}
module Platform.DB.Marshaller (
    userIDField
  , userNameField
  , emailField
  , passwordField
  , createdAtField
  , updatedAtField
  , userMarshaller
) where

import Platform.DB.Model
import Orville.PostgreSQL
import Data.Text (Text)
import Data.Time (UTCTime)

userIDField :: FieldDefinition NotNull UserID
userIDField = coerceField $ serialField "user_id"

userNameField :: FieldDefinition NotNull Text
userNameField = boundedTextField "user_name" 255

emailField :: FieldDefinition NotNull Text
emailField = boundedTextField "email" 255

passwordField :: FieldDefinition NotNull Text
passwordField = unboundedTextField "password"

createdAtField :: FieldDefinition NotNull UTCTime
createdAtField = utcTimestampField "created_at"

updatedAtField :: FieldDefinition NotNull UTCTime
updatedAtField = utcTimestampField "updated_at"

userMarshaller :: SqlMarshaller UserWrite UserRead 
userMarshaller = 
    User
        <$> marshallReadOnly (marshallField (\User{..} -> userID) 
                userIDField)
        <*> marshallField (\User{..} -> userName) userNameField
        <*> marshallField (\User{..} -> email) emailField 
        <*> marshallField (\User{..} -> password) passwordField
        <*> marshallReadOnly (marshallField (\User{..} -> createdAt) 
                createdAtField)
        <*> marshallReadOnly (marshallField (\User{..} -> updatedAt) 
                updatedAtField)
