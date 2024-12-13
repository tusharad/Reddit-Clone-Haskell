{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Platform.User.Thread.VoteThread.Types (
    VoteThreadResponse(..),
    RemoveVoteThreadResponse(..),
    FetchVoteThreadsForUserReq(..),
    FetchVoteThreadsForUserResponse(..)
) where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Platform.DB.Model (ThreadID)

newtype RemoveVoteThreadResponse = RemoveVoteThreadResponse {
    removeVoteThreadResponseMsg :: Text
} deriving (Show, Eq, Generic, ToJSON)

newtype VoteThreadResponse = VoteThreadResponse {
    voteThreadResponseMsg :: Text
} deriving (Show, Eq, Generic, ToJSON)

newtype FetchVoteThreadsForUserResponse = FetchVoteThreadsForUserResponse [(ThreadID,Bool)]
    deriving (Show, Eq, Generic, ToJSON)

newtype FetchVoteThreadsForUserReq = FetchVoteThreadsForUserReq {
  threadListForVotes :: [ThreadID]
 }
    deriving (Show, Eq, Generic, FromJSON)
