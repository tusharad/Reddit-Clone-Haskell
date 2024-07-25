{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Platform.API
  ( MainAPI,
    mainServer,
  )
where

import Platform.Admin.Types
import Platform.Auth.Handler
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Handler
import Platform.User.Handler
import Platform.User.Types
import Servant
import Servant.Auth.Server
import Servant.Multipart
import UnliftIO

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

type MainAPI auths =
  CheckHealthAPI
    :<|> RegisterUserAPI
    :<|> LoginUserAPI
    :<|> AdminLoginAPI
    :<|> Auth auths UserInfo :> UserDashboard
    :<|> Auth auths UserInfo :> UserChangePasswordAPI
    :<|> Auth auths UserInfo :> DeleteUserAPI
    :<|> Auth auths UserInfo :> UpdateUserImageAPI

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
