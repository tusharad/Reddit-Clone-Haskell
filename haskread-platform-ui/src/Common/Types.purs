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

data Endpoint = Threads | UserByToken | Login0 | Register0 | VerifyOtp0 Int Int
derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum { 
    "Threads" : "api" / "v1" / "thread" / "all" / noArgs ,
    "UserByToken" : "api" / "v1" / "user" / "profile" / noArgs,
    "Login0" : "api" / "v1" / "user" / "auth" / "login" / noArgs,
    "Register0" : "api" / "v1" / "user" / "auth" / "register" / noArgs,
    "VerifyOtp0" : "api" / "v1" / "user" / "auth" / "verify" / (int segment) / (int segment)
    }

data RequestMethod = 
    Get | Post (Maybe Json) | Put (Maybe Json) | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

data MyRoute =
      Home
    | Login
    | Register
    | OTP Int

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
    }

type PaginatedArray a =
  { total :: Int
  , body :: Array a
  }

-- Thread
type Thread = {
    title :: String,
    description :: Maybe String
  }

threadCodec :: JsonCodec Thread
threadCodec =
    CAR.object "Thread" {
        title : CA.string,
        description : CAC.maybe CA.string
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


type Profile = {
        userID :: Int,
        userName :: String
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
