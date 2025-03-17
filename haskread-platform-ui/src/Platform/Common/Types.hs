{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.Common.Types
  ( UserProfileResponse (..)
  , CreateThreadData (..)
  , AddCommentData (..)
  , AppColor (..)
  , ToColor (..)
  -- , ClassName
  , ThreadInfo (..)
  , FetchAllThreadsResponse (..)
  , Communities (..)
  , CommunityC (..)
  , LoginUserBody (..)
  , LoginUserResponse (..)
  , RegisterUserBody (..)
  , RegisterUserResponse (..)
  , CommentInfo (..)
  , NestedComment (..)
  , FetchCommentsResponse (..)
  , CreateCommentReqBody (..)
  , FetchVoteThreadsForUserReq (..)
  , FetchVoteComemntsForUserReq (..)
  , FetchVoteComments (..)
  , FetchVoteCommentsForUserResponse (..)
  , CreateThreadReqBody (..)
  , EditThreadData (..)
  , UpdateThreadReqBody (..)
  , UpdateCommentReqBody (..)
  , ChangePasswordBody (..)
  , DeleteUserBody (..)
  , AuthData (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String.Conversions
import Data.Text (Text)
import Data.Time
import Text.Read (readMaybe)
import Web.HttpApiData
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData (Param (..))

-- import Web.View.Types (ClassName)

data UserProfileResponse = UserProfileResponse
  { userIDForUPR :: Int
  , userNameForUPR :: Text
  , userEmail :: Text
  , userCreatedAt :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, Read)

data CreateThreadData = CreateThreadData
  { mToken :: Maybe Text
  , mUserInfo :: Maybe UserProfileResponse
  , communityId :: Int
  , titleForCreateThread :: Text
  , content :: Text
  }
  deriving (Eq, Show, Read)

data EditThreadData = EditThreadData
  { threadIdForEditThread :: Int
  , titleForEditThread :: Maybe Text
  , userIdForEditThread :: Int
  , descriptionForEditThread :: Maybe Text
  , communityIdForEditThread :: Maybe Int
  }
  deriving (Eq, Show, Read)

data AddCommentData = AddCommentData
  { contentForAddComment :: Text
  , threadId :: Int
  , userToken :: Text
  , parentCommentIdForAddComment :: Maybe Int
  }
  deriving (Eq, Show, Read)

data ThreadInfo = ThreadInfo
  { threadIDForThreadInfo :: Int
  , title :: Text
  , description :: Maybe Text
  , attachmentName :: Maybe Text
  , attachmentSize :: Maybe Int
  , createdAtForThreadInfo :: Text
  , userIDForThreadInfo :: Int
  , userNameForThreadInfo :: Text
  , communityIDForThreadInfo :: Int
  , communityNameForThreadInfo :: Text
  , upvoteCount :: Maybe Int
  , downvoteCount :: Maybe Int
  , commentCount :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromJSON, Read)

data FetchAllThreadsResponse = FetchAllThreadsResponse
  { threadsCount :: Int
  , threads :: [ThreadInfo]
  }
  deriving (Show, Eq, Generic, FromJSON)

data AppColor
  = White
  | Light
  | GrayLight
  | GrayDark
  | Dark
  | Success
  | Danger
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show, Read)

instance ToHttpApiData AppColor where
  toQueryParam c = cs (show c)

instance FromHttpApiData AppColor where
  parseQueryParam t = do
    case readMaybe (cs t) of
      Nothing -> Left $ "Invalid AppColor: " <> t
      (Just c) -> pure c

instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Light = "#F2F2F3"
  colorValue GrayLight = "#E3E5E9"
  colorValue GrayDark = "#2С3С44"
  colorValue Dark = "#2E3842" -- "#232C41"
  colorValue Primary = "#4171b7"
  colorValue PrimaryLight = "#6D9BD3"
  colorValue Secondary = "#5D5A5C"
  colorValue SecondaryLight = "#9D999C"
  -- colorValue Success = "67C837"
  colorValue Success = "#149e5a"
  colorValue Danger = "#ef1509"
  colorValue Warning = "#e1c915"

data Communities = Communities
  { communities :: [CommunityC]
  }
  deriving (Show, Eq, Generic, FromJSON)

data CommunityC = CommunityC
  { communityName :: Text
  , communityID :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

data LoginUserBody = LoginUserBody
  { emailForLogin :: Text
  , passwordForLogin :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data LoginUserResponse = LoginUserResponse
  { jwtToken :: Text
  , loginUserResponseMsg :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

data RegisterUserBody = RegisterUserBody
  { userNameForRegister :: Text
  , emailForRegister :: Text
  , passwordForRegister :: Text
  , confirmPasswordForRegister :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RegisterUserResponse = RegisterUserResponse
  { userIDForRUR :: Int
  , registerUserResponseMessage :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CommentInfo = CommentInfo
  { commentIDForCommentInfo :: Int
  , commentContentForCommentInfo :: Text
  , userIDForCommentInfo :: Int
  , userNameForCommentInfo :: Text
  , threadIDForCommentInfo :: Int
  , createdAtForCommentInfo :: UTCTime
  , parentCommentIDForCommentInfo :: Maybe Int
  , commentUpvoteCount :: Maybe Int
  , commentDownvoteCount :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromJSON, Read)

newtype UpdateCommentReqBody = UpdateCommentReqBody
  {commentContentForUpdate :: Text}
  deriving (Generic, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

data NestedComment = NestedComment
  { mainComment :: CommentInfo
  , children :: [NestedComment]
  }
  deriving (Show, Eq, Generic, FromJSON)

data FetchCommentsResponse = FetchCommentsResponse
  { commentsCount :: Int
  , comments :: [NestedComment]
  }
  deriving (Show, Eq, Generic, FromJSON)

data CreateCommentReqBody = CreateCommentReqBody
  { threadIDForCommentCreate :: Int
  , commentContentForCreate :: Text
  , parentCommentIDForCreate :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FetchVoteThreadsForUserReq = FetchVoteThreadsForUserReq
  { threadListForVotes :: [Int]
  }
  deriving (Show, Eq, Generic, ToJSON)

data FetchVoteComemntsForUserReq = FetchVoteComemntsForUserReq
  { commentListForVotes :: [Int]
  }
  deriving (Show, Eq, Generic, ToJSON)

data FetchVoteComments = FetchVoteComments
  { commentIDForFetchVote :: Int
  , isUpvote :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON)

{-
data CVote = CVote { commentIDForFetchVote :: Int, isUpvote :: Bool  }
    deriving (Show, Eq, Generic, FromJSON)
newtype CVoteList = CVoteList [CVote]
    deriving (Show, Eq, Generic, FromJSON)
-}

data FetchVoteCommentsForUserResponse = FetchVoteCommentsForUserResponse
  { fetchVoteCommentsList :: [FetchVoteComments]
  }
  deriving (Show, Eq, Generic, FromJSON)

data CreateThreadReqBody = CreateThreadReqBody
  { threadTitleForCreate :: Text
  , threadDescriptionForCreate :: Maybe Text
  , threadCommunityIDForCreate :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data UpdateThreadReqBody = UpdateThreadReqBody
  { threadIDForUpdate :: Int
  , threadTitleForUpdate :: Text
  , threadDescriptionForUpdate :: Maybe Text
  , threadCommunityIDForUpdate :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ChangePasswordBody = ChangePasswordBody
  { oldPasswordForChangePass :: Text
  , newPasswordForChangePass :: Text
  , confirmPasswordForChangePass :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DeleteUserBody = DeleteUserBody
  { passwordForDeleteUser :: Text
  , areUSure :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AuthData = AuthData
  { jToken :: Maybe Text
  }
  deriving (Generic, Show, Read, ToParam, FromParam)

instance Session AuthData where
  sessionKey = Param "AuthData"
  cookiePath = Just []

instance DefaultParam AuthData where
  defaultParam = AuthData Nothing
