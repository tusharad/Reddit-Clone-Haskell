{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Platform.Comment.Types
  ( CreateCommentReqBody (..),
    CreateCommentResponse (..),
    DeleteCommentResponse (..),
    UpdateCommentReqBody (..),
    UpdateCommentResponse (..),
    VoteCommentResponse (..),
    FetchCommentsResponse (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Platform.DB.Model
import Servant (FromHttpApiData)

data CreateCommentReqBody = CreateCommentReqBody
  { threadIDForCommentCreate :: ThreadID,
    commentContentForCreate :: Text,
    parentCommentID :: Maybe CommentID
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype CreateCommentResponse = CreateCommentResponse
  {createCommentResponseMsg :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

newtype DeleteCommentResponse = DeleteCommentResponse
  {deleteCommentResponseMsg :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

newtype UpdateCommentReqBody = UpdateCommentReqBody
  {commentContentForUpdate :: Text}
  deriving (Generic, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

newtype UpdateCommentResponse = UpdateCommentResponse
  {updateCommentResponseMsg :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

newtype VoteCommentResponse = VoteCommentResponse
  { voteCommentResponseMsg :: Text
  }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

data FetchCommentsResponse = FetchCommentsResponse
  { commentsCount :: Int,
    comments :: [NestedComment]
  }
  deriving (Show, Eq, Generic, ToJSON)
