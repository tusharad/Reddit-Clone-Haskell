{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Platform.API
  ( MainAPI,
    mainServer,
  )
where

import Platform.Handler
import Platform.Admin.Handler
import Platform.Admin.Community.Handler
import Platform.User.Handler
import Platform.Auth.Handler

import Platform.Admin.Types
import Platform.Auth.Types
import Platform.User.Types
import Platform.Admin.Community.Types

import Platform.Common.AppM

import Servant
import Servant.Auth.Server
import Servant.Multipart
import UnliftIO
import Platform.DB.Model

mainServer :: (MonadUnliftIO m) => CookieSettings -> JWTSettings -> ServerT (MainAPI auths) (AppM m)
mainServer cookieSett jwtSett =
  checkHealthH
    :<|> registerUserH
    :<|> loginUserH cookieSett jwtSett
    :<|> adminLoginH cookieSett jwtSett
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

type MainAPI auths =
  CheckHealthAPI
    :<|> RegisterUserAPI
    :<|> LoginUserAPI
    :<|> AdminLoginAPI
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