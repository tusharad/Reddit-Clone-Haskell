{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Platform.API
  ( MainAPI,
    mainServer,
  )
where

import Data.Text (Text)
import GHC.Int (Int32)
import Platform.Admin.Handler
import Platform.Admin.Types
import Platform.Auth.Handler
import Platform.Auth.Types
import Platform.Comment.Handler
import Platform.Comment.Types
import Platform.Common.AppM
import Platform.Community.Handler
import Platform.Community.Types
import Platform.DB.Model
import Platform.Handler
import Platform.User.Handler
import Platform.User.Thread.Handler
import Platform.User.Thread.Types
import Platform.User.Thread.VoteThread.Handler
import Platform.User.Thread.VoteThread.Types
import Platform.User.Types
import Servant
import Servant.Auth.Server
import Servant.Multipart
import UnliftIO
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Media ((//))

mainServer :: (MonadUnliftIO m) => ServerT (MainAPI auths) (AppM m)
mainServer =
  checkHealthH
    :<|> registerUserH
    :<|> loginUserH
    :<|> adminLoginH
    :<|> verifyEmailH
    :<|> resendVerifyEmailH
    :<|> userDashboardH
    :<|> userChangePasswordH
    :<|> userDeleteAccountH
    :<|> userUpdateProfileImageH
    :<|> adminDashboardH
    :<|> adminChangePasswordH
    :<|> adminCreateAdminH
    :<|> communityCreateH
    :<|> communityUpdateH
    :<|> communityDeleteH
    :<|> createThreadH
    :<|> updateThreadH
    :<|> deleteThreadH
    :<|> voteThreadH True
    :<|> voteThreadH False
    :<|> createCommentH
    :<|> deleteCommentH
    :<|> updateCommentH
    :<|> voteCommentH
    :<|> oauth2LoginH
    :<|> oauth2CallbackH
    :<|> fetchVoteThreadsForUserH
    :<|> fetchVoteCommentsForUserH
    :<|> fetchAllThreadsH
    :<|> fetchThreadH
    :<|> fetchCommentsByThreadH
    :<|> fetchCommunitiesH
    :<|> fetchAllThreadsBySearchH
    :<|> fetchUserProfileImageH
    :<|> fetchThreadAttachmentH

type MainAPI auths =
  CheckHealthAPI
    :<|> RegisterUserAPI
    :<|> LoginUserAPI
    :<|> AdminLoginAPI
    :<|> VerifyEmailAPI
    :<|> ResendVerifyEmailAPI
    :<|> Auth auths UserInfo :> UserDashboard
    :<|> Auth auths UserInfo :> UserChangePasswordAPI
    :<|> Auth auths UserInfo :> DeleteUserAPI
    :<|> Auth auths UserInfo :> UpdateUserImageAPI
    :<|> Auth auths AdminInfo :> AdminDashboardAPI
    :<|> Auth auths AdminInfo :> AdminChangePasswordAPI
    :<|> Auth auths AdminInfo :> AdminCreateAdminAPI
    :<|> Auth auths AdminInfo :> CommunityCreateAPI
    :<|> Auth auths AdminInfo :> CommunityUpdateAPI
    :<|> Auth auths AdminInfo :> DeleteCommunityAPI
    :<|> Auth auths UserInfo :> CreateThreadAPI
    :<|> Auth auths UserInfo :> UpdateThreadAPI
    :<|> Auth auths UserInfo :> DeleteThreadAPI
    :<|> Auth auths UserInfo :> UpvoteThreadAPI
    :<|> Auth auths UserInfo :> DownvoteThreadAPI
    :<|> Auth auths UserInfo :> CreateCommentAPI
    :<|> Auth auths UserInfo :> DeleteCommentAPI
    :<|> Auth auths UserInfo :> UpdateCommentAPI
    :<|> Auth auths UserInfo :> CommentVoteAPI
    :<|> Auth auths UserInfo :> OAuth2LoginAPI
    :<|> Auth auths UserInfo :> OAuth2CallBackAPI
    :<|> Auth auths UserInfo :> FetchVoteThreadsForUser
    :<|> Auth auths UserInfo :> FetchVoteCommentsForUser
    :<|> FetchAllThreadsAPI
    :<|> FetchThreadAPI
    :<|> FetchCommentsByThreadAPI
    :<|> FetchCommunitiesAPI
    :<|> FetchAllThreadsBySearch 
    :<|> GetProfileImage
    :<|> GetThreadAttachment

type CheckHealthAPI = "check-health" :> Get '[JSON] String

-- User APIs
type RegisterUserAPI =
  "api"
    :> "v1"
    :> "user"
    :> "auth"
    :> "register"
    :> ReqBody '[JSON] RegisterUserBody
    :> Post '[JSON] RegisterUserResponse

type LoginUserAPI =
  "api"
    :> "v1"
    :> "user"
    :> "auth"
    :> "login"
    :> ReqBody '[JSON] LoginUserBody
    :> Post
         '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             LoginUserResponse
         )

type UserDashboard =
  "api"
    :> "v1"
    :> "user"
    :> "profile"
    :> Get '[JSON] UserProfileResponse

type UserChangePasswordAPI =
  "api"
    :> "v1"
    :> "user"
    :> "profile"
    :> "change-password"
    :> ReqBody '[JSON] ChangePasswordBody
    :> Put '[JSON] ChangePasswordResponse

type DeleteUserAPI =
  "api"
    :> "v1"
    :> "user"
    :> "profile"
    :> "delete-account"
    :> ReqBody '[JSON] DeleteUserBody
    :> Delete '[JSON] DeleteUserResponse

type UpdateUserImageAPI =
  "api"
    :> "v1"
    :> "user"
    :> "profile"
    :> "update-image"
    :> MultipartForm Mem UpdateUserImageBody
    :> Put '[JSON] UpdateUserImageResponse

-- Admin APIs

type AdminLoginAPI =
  "api"
    :> "v1"
    :> "admin"
    :> "auth"
    :> "login"
    :> ReqBody '[JSON] AdminLoginBodyReq
    :> Post
         '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             AdminLoginResponse
         )

type AdminDashboardAPI =
  "api"
    :> "v1"
    :> "admin"
    :> "dashboard"
    :> Get '[JSON] AdminDashboardResponse

type AdminChangePasswordAPI =
  "api"
    :> "v1"
    :> "admin"
    :> "profile"
    :> "change-password"
    :> ReqBody '[JSON] AdminChangePasswordBody
    :> Put '[JSON] AdminChangePasswordResponse

type AdminCreateAdminAPI =
  "api"
    :> "v1"
    :> "admin"
    :> "create-admin"
    :> ReqBody '[JSON] AdminCreateAdminReqBody
    :> Post '[JSON] AdminCreateAdminResponse

-- Community APIs

type CommunityCreateAPI =
  "api"
    :> "v1"
    :> "admin"
    :> "community"
    :> "create"
    :> ReqBody '[JSON] CommunityCreateReqBody
    :> Post '[JSON] CommunityCreateResponse

type CommunityUpdateAPI =
  "api"
    :> "v1"
    :> "admin"
    :> "community"
    :> "update"
    :> ReqBody '[JSON] CommunityUpdateReqBody
    :> Put '[JSON] CommunityUpdateResponse

type DeleteCommunityAPI =
  "api"
    :> "v1"
    :> "admin"
    :> "community"
    :> "delete"
    :> Capture "communityID" CommunityID
    :> Delete '[JSON] CommunityDeleteResponse

-- Thread APIs

type CreateThreadAPI =
  "api"
    :> "v1"
    :> "user"
    :> "thread"
    :> "create"
    :> MultipartForm Mem CreateThreadReqBody
    :> Post '[JSON] CreateThreadResponse

type UpdateThreadAPI =
  "api"
    :> "v1"
    :> "user"
    :> "thread"
    :> "update"
    :> MultipartForm Mem UpdateThreadReqBody
    :> Put '[JSON] UpdateThreadResponse

type DeleteThreadAPI =
  "api"
    :> "v1"
    :> "user"
    :> "thread"
    :> "delete"
    :> Capture "threadID" ThreadID
    :> Delete '[JSON] DeleteThreadResponse

-- ThreadVote APIs

type UpvoteThreadAPI =
  "api"
    :> "v1"
    :> "user"
    :> "thread"
    :> "upvote"
    :> Capture "threadID" ThreadID
    :> Post '[JSON] VoteThreadResponse

type DownvoteThreadAPI =
  "api"
    :> "v1"
    :> "user"
    :> "thread"
    :> "downvote"
    :> Capture "threadID" ThreadID
    :> Post '[JSON] VoteThreadResponse

-- Comment APIs

type CreateCommentAPI =
  "api"
    :> "v1"
    :> "user"
    :> "comment"
    :> "create"
    :> ReqBody '[JSON] CreateCommentReqBody
    :> Post '[JSON] CreateCommentResponse

type DeleteCommentAPI =
  "api"
    :> "v1"
    :> "user"
    :> "comment"
    :> "delete"
    :> Capture "commentID" CommentID
    :> Delete '[JSON] DeleteCommentResponse

type UpdateCommentAPI =
  "api"
    :> "v1"
    :> "user"
    :> "comment"
    :> "update"
    :> Capture "commentID" CommentID
    :> ReqBody '[JSON] UpdateCommentReqBody
    :> Put '[JSON] UpdateCommentResponse

type CommentVoteAPI =
  "api"
    :> "v1"
    :> "user"
    :> "comment"
    :> "vote"
    :> Capture "commentID" CommentID
    :> Capture "vote" Bool
    :> Put '[JSON] VoteCommentResponse

type VerifyEmailAPI =
  "api"
    :> "v1"
    :> "user"
    :> "auth"
    :> "verify"
    :> Capture "UserID" UserID
    :> Capture "Token" Int32
    :> Put '[JSON] VerifyEmailResponse

type ResendVerifyEmailAPI =
  "api"
    :> "v1"
    :> "user"
    :> "auth"
    :> "verify"
    :> "resend"
    :> Capture "UserID" UserID
    :> Put '[JSON] ResendVerifyEmailResponse

type OAuth2LoginAPI =
  "api"
    :> "v1"
    :> "user"
    :> "oauth2"
    :> "login"
    :> Get '[JSON] NoContent

type OAuth2CallBackAPI =
  "callback"
    :> QueryParam "state" Text
    :> QueryParam "code" Text
    :> Get
         '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             LoginUserResponse
         )

type FetchAllThreadsAPI =
  "api"
    :> "v1"
    :> "thread"
    :> "all"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "communityId" Int
    :> QueryParam "userId" Int
    :> Get '[JSON] FetchAllThreadsResponse

type FetchThreadAPI =
  "api"
    :> "v1"
    :> "thread"
    :> Capture "ThreadID" ThreadID
    :> Get '[JSON] ThreadInfo

type FetchCommentsByThreadAPI =
  "api"
    :> "v1"
    :> "thread"
    :> "comment"
    :> Capture "ThreadID" ThreadID
    :> Get '[JSON] FetchCommentsResponse

type FetchCommunitiesAPI =
  "api"
    :> "v1"
    :> "community"
    :> Get '[JSON] FetchCommunitiesResponse

type FetchVoteThreadsForUser = 
    "api"
        :> "v1"
        :> "user"
        :> "thread_votes"
        :> ReqBody '[JSON] FetchVoteThreadsForUserReq
        :> Post '[JSON] FetchVoteThreadsForUserResponse

type FetchVoteCommentsForUser = 
    "api"
        :> "v1"
        :> "user"
        :> "comment_votes"
        :> ReqBody '[JSON] FetchVoteComemntsForUserReq
        :> Post '[JSON] FetchVoteCommentsForUserResponse

type FetchAllThreadsBySearch = 
  "api" 
    :> "v1"
    :> "user"
    :> "thread"
    :> QueryParam "search_term" Text
    :> Get '[JSON] FetchAllThreadsResponse

data ImagePNG

instance Accept ImagePNG where
  contentType _ = "image" // "png"

instance MimeRender ImagePNG LBS.ByteString where
  mimeRender _ val = val

-- API type definition
type GetProfileImage = "api" 
    :> "v1"
    :> "user"
    :> "profile-image"
    :> Capture "UserID" UserID
    :> Get '[ImagePNG] LBS.ByteString

type GetThreadAttachment = "api" 
    :> "v1"
    :> "thread"
    :> "attachment"
    :> Capture "ThreadID" ThreadID
    :> Get '[OctetStream] LBS.ByteString
