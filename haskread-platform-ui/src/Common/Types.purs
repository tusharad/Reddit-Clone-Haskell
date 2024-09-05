module Common.Types
  ( BaseURL(..)
  , Endpoint(..)
  , MyRoute(..)
  , PaginatedArray
  , RequestMethod(..)
  , RequestOptions
  , Thread
  , Token(..)
  , endpointCodec
  , myRoute
  , threadCodec
  , threadsCodec
  , profileCodec 
  , Profile
  , LoginFields
  , RegisterFields
  , loginCodec
  , registerCodec
  , OtpFields
  , CreateThreadFields 
  , createThreadCodec
  , changePasswordCodec 
  , ChangePasswordFields
  , DeleteUserFields
  , deleteUserCodec 
  , updateThreadCodec
  , UpdateThreadFields 
  )
  where

import Prelude hiding ((/))

import Data.Argonaut.Core (Json)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', path, root,int,segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))

newtype BaseURL = BaseURL String

data Endpoint = Threads 
                | UserByToken 
                | Login0 
                | Register0 
                | VerifyOtp0 Int Int
                | CreateThread0
                | ChangePassword0
                | DeleteThread0 Int
                | DeleteUser0
                | UpdateThread0
                | GetThreadByID0 Int

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum { 
     "Threads" : "api" / "v1" / "thread" / "all" / noArgs
   , "UserByToken" : "api" / "v1" / "user" / "profile" / noArgs
   , "Login0" : "api" / "v1" / "user" / "auth" / "login" / noArgs
   , "Register0" : "api" / "v1" / "user" / "auth" / "register" / noArgs
   , "VerifyOtp0" : "api" / "v1" / "user" / "auth" / "verify" / (int segment) / (int segment)
   , "CreateThread0" : "api" / "v1" / "user" / "thread" / "create" / noArgs
   , "ChangePassword0" : "api" / "v1" / "user" / "profile" / "change-password" / noArgs
   , "DeleteThread0" : "api" / "v1" / "user" / "thread" / "delete" / (int segment)
   , "DeleteUser0" : "api" / "v1" / "user" / "profile" / "delete-account" / noArgs
   , "UpdateThread0" : "api" / "v1" / "user" / "thread" / "update" / noArgs
   , "GetThreadByID0" : "api" / "v1" / "thread" / (int segment)
    }

data RequestMethod = 
    Get | Post (Maybe Json) | Put (Maybe Json) | Delete (Maybe Json)

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

data MyRoute =
      Home
    | Login
    | Register
    | OTP Int
    | CreateThread
    | ChangePassword
    | DeleteUser
    | UpdateThread Int

derive instance genericRoute :: Generic MyRoute _
derive instance eqRoute :: Eq MyRoute

newtype Token = Token String

derive instance eqToken :: Eq Token
instance showToken :: Show Token where
    show (Token _) = "TOKEN"

myRoute :: RouteDuplex' MyRoute
myRoute = root $ G.sum {
        "Home" : G.noArgs
      , "Login" : path "login" G.noArgs
      , "Register" : path "register" G.noArgs
      , "OTP" : "otp" / (int segment)
      , "CreateThread" : path "create-thread" G.noArgs
      , "ChangePassword" : path "change-password" G.noArgs
      , "DeleteUser" : path "delete-my-account" G.noArgs
      , "UpdateThread" : "update-thread" / (int segment)
    }

type PaginatedArray a =
  { total :: Int
  , body :: Array a
  }

-- Types for Fetching and inserting
type Thread = {
    title :: String,
    description :: Maybe String,
    communityIDForThreadInfo :: Int,
    userIDForThreadInfo :: Int,
    communityNameForThreadInfo :: String,
    createdAtForThreadInfo :: String,
    downvoteCount :: Maybe Int,
    threadIDForThreadInfo :: Int,
    upvoteCount :: Maybe Int,
    userNameForThreadInfo :: String
  }

type Profile = {
        userID :: Int,
        userName :: String
    }

type OtpFields = {
        otp :: Int,
        userID :: Int
    }

type LoginFields =
  { emailForLogin :: String
  , passwordForLogin :: String
  }

type RegisterFields = {
    userNameForRegister :: String
   , emailForRegister :: String
   , passwordForRegister :: String
   , confirmPasswordForRegister :: String
}

type CreateThreadFields = {
    threadTitleForCreate :: String,
    threadDescriptionForCreate :: String, 
    threadCommunityIDForCreate :: Int
}

type ChangePasswordFields = {
        oldPasswordForChangePass :: String,
        newPasswordForChangePass :: String,
        confirmPasswordForChangePass :: String
    }

type DeleteUserFields = {
        passwordForDeleteUser :: String,
        areUSure :: Boolean
    }

type UpdateThreadFields = {
        threadIDForUpdate :: Int,
        threadTitleForUpdate :: String,
        threadDescriptionForUpdate :: String,
        threadCommunityIDForUpdate :: Int
    }

profileCodec :: JsonCodec Profile
profileCodec =
  CAM.renameField "userIDForUPR" "userID"
  >~> CAM.renameField "userNameForUPR" "userName"
  >~>
  (CAR.object "Profile" { 
       userID : CA.int,
       userName: CA.string
    })

threadCodec :: JsonCodec Thread
threadCodec =
    CAR.object "Thread" {
        threadIDForThreadInfo : CA.int
      , title : CA.string
      , description : CAC.maybe CA.string
      , userIDForThreadInfo : CA.int
      , userNameForThreadInfo : CA.string
      , communityIDForThreadInfo : CA.int
      , communityNameForThreadInfo : CA.string
      , createdAtForThreadInfo : CA.string
      , upvoteCount: CAC.maybe CA.int
      , downvoteCount: CAC.maybe CA.int
    }

threadsCodec :: JsonCodec (PaginatedArray Thread)
threadsCodec = 
    CAM.renameField "threads" "body"
    >~> CAM.renameField "threadsCount" "total"
    >~> codec
  where
  codec =
    CAR.object "PaginatedArray Thread"
      { body: CA.array threadCodec
      , total: CA.int
      }

loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginFields"
    { emailForLogin: CA.string
    , passwordForLogin: CA.string
    }

registerCodec :: JsonCodec RegisterFields
registerCodec =
  CAR.object "LoginFields"
    { userNameForRegister: CA.string
    , emailForRegister : CA.string
   , passwordForRegister : CA.string
   , confirmPasswordForRegister : CA.string
    }

createThreadCodec :: JsonCodec CreateThreadFields
createThreadCodec =
    CAR.object "create thread fields" {
    threadTitleForCreate : CA.string,
    threadDescriptionForCreate : CA.string, 
    threadCommunityIDForCreate : CA.int
    }

changePasswordCodec :: JsonCodec ChangePasswordFields
changePasswordCodec = 
    CAR.object "change password fields" {
        oldPasswordForChangePass : CA.string,
        newPasswordForChangePass : CA.string,
        confirmPasswordForChangePass : CA.string
    }

deleteUserCodec :: JsonCodec DeleteUserFields
deleteUserCodec =
    CAR.object "Delete user" {
        passwordForDeleteUser : CA.string,
        areUSure : CA.boolean
    }

updateThreadCodec :: JsonCodec UpdateThreadFields
updateThreadCodec = 
 CAR.object "update thread" {
    threadIDForUpdate : CA.int,
    threadTitleForUpdate : CA.string,
    threadDescriptionForUpdate : CA.string,
    threadCommunityIDForUpdate : CA.int
    }
