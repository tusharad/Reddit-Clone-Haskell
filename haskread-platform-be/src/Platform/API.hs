{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
    :> MultipartForm Tmp UpdateUserImageBody
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
    :> ReqBody '[JSON] CreateThreadReqBody
    :> Post '[JSON] CreateThreadResponse

type UpdateThreadAPI =
  "api"
    :> "v1"
    :> "user"
    :> "thread"
    :> "update"
    :> ReqBody '[JSON] UpdateThreadReqBody
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
