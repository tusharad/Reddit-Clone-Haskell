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
    FetchVoteComemntsForUserReq (..),
    FetchVoteCommentsForUserResponse (..),
    FetchVoteComments (..)
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
    parentCommentIDForCreate :: Maybe CommentID
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

newtype FetchVoteComemntsForUserReq = FetchVoteComemntsForUserReq {
    commentListForVotes :: [CommentID]
  } deriving newtype (Show, Eq, FromJSON)

data FetchVoteComments = FetchVoteComments {
    commentIDForFetchVote :: CommentID
  , isUpvote :: Bool
} deriving (Show, Eq, Generic, ToJSON)

newtype FetchVoteCommentsForUserResponse = FetchVoteCommentsForUserResponse {
    fetchVoteCommentsList :: [FetchVoteComments]
} deriving newtype (Show, Eq, ToJSON)
