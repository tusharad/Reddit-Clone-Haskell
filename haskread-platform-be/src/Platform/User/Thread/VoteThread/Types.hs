{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Platform.User.Thread.VoteThread.Types (
    VoteThreadResponse(..),
    RemoveVoteThreadResponse(..),
    FetchUserThreadVotesResponse(..),
    FetchUserThreadVotesReq(..)
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

newtype FetchUserThreadVotesResponse = FetchUserThreadVotesResponse [(ThreadID,Bool)]
    deriving (Show, Eq, Generic, ToJSON)

newtype FetchUserThreadVotesReq = FetchUserThreadVotesReq [ThreadID]
    deriving (Show, Eq, Generic, FromJSON)
