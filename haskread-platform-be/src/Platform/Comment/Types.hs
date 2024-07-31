{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Platform.Comment.Types (
    CreateCommentReqBody(..),
    CreateCommentResponse(..),
    DeleteCommentResponse(..),
    UpdateCommentReqBody(..),
    UpdateCommentResponse(..)
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Platform.DB.Model
import Servant (FromHttpApiData)

data CreateCommentReqBody = CreateCommentReqBody
  { threadIDForCommentCreate :: ThreadID,
    commentContentForCreate :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype CreateCommentResponse = CreateCommentResponse
  { createCommentResponseMsg :: Text }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

newtype DeleteCommentResponse = DeleteCommentResponse
  { deleteCommentResponseMsg :: Text }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

data UpdateCommentReqBody = UpdateCommentReqBody
  { commentContentForUpdate :: Text }
  deriving (Generic,Show, Eq, Ord, ToJSON, FromJSON)

newtype UpdateCommentResponse = UpdateCommentResponse
  { updateCommentResponseMsg :: Text }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)
