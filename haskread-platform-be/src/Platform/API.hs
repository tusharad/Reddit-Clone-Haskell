{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Platform.API
  ( MainAPI,
    mainServer,
  )
where

import Platform.Auth.Handler
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Handler
import Platform.User.Handler
import Platform.User.Types
import Servant
import Servant.Auth.Server
import UnliftIO

mainServer :: (MonadUnliftIO m) => CookieSettings -> JWTSettings -> ServerT (MainAPI auths) (AppM m)
mainServer cookieSett jwtSett =
  checkHealthH
    :<|> registerUserH
    :<|> loginUserH cookieSett jwtSett
    :<|> userDashboardH
    :<|> userChangePasswordH

type MainAPI auths =
  CheckHealthAPI
    :<|> RegisterUserAPI
    :<|> LoginUserAPI
    :<|> Auth auths UserInfo :> UserDashboard
    :<|> Auth auths UserInfo :> UserChangePasswordAPI

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
