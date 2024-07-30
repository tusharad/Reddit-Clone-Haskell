{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Platform.User.Thread.VoteThread.Types (
    VoteThreadResponse(..),
    RemoveVoteThreadResponse(..)
) where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson

newtype RemoveVoteThreadResponse = RemoveVoteThreadResponse {
    removeVoteThreadResponseMsg :: Text
} deriving (Show, Eq, Generic, ToJSON)

newtype VoteThreadResponse = VoteThreadResponse {
    voteThreadResponseMsg :: Text
} deriving (Show, Eq, Generic, ToJSON)
