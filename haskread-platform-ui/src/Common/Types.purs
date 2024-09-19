module Common.Types
  ( BaseURL(..)
  , ChangePasswordFields
  , CommentInfo(..)
  , Community(..)
  , CommunityRep
  , CreateThreadFields
  , DeleteUserFields
  , Endpoint(..)
  , LoginFields
  , MyRoute(..)
  , NestedComment(..)
  , OtpFields
  , PaginatedArray
  , Pagination(..)
  , Profile
  , RegisterFields
  , RequestMethod(..)
  , RequestOptions
  , Thread
  , ThreadInfo(..)
  , ThreadRep
  , Token(..)
  , UpdateThreadFields
  , changePasswordCodec
  , communitiesCodec
  , createThreadCodec
  , deleteUserCodec
  , endpointCodec
  , loginCodec
  , myRoute
  , nestedCommentsCodec
  , profileCodec
  , registerCodec
  , threadCodec
  , threadsCodec
  , updateThreadCodec
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
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Routing.Duplex (RouteDuplex', path, root, int, segment,optional, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/), (?))
import Undefined (undefined)

newtype BaseURL = BaseURL String

data Endpoint
  = Threads
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
  | Comments Int
  | Community

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Threads": "api" / "v1" / "thread" / "all" / noArgs
  , "UserByToken": "api" / "v1" / "user" / "profile" / noArgs
  , "Login0": "api" / "v1" / "user" / "auth" / "login" / noArgs
  , "Register0": "api" / "v1" / "user" / "auth" / "register" / noArgs
  , "VerifyOtp0": "api" / "v1" / "user" / "auth" / "verify" / (int segment) / (int segment)
  , "CreateThread0": "api" / "v1" / "user" / "thread" / "create" / noArgs
  , "ChangePassword0": "api" / "v1" / "user" / "profile" / "change-password" / noArgs
  , "DeleteThread0": "api" / "v1" / "user" / "thread" / "delete" / (int segment)
  , "DeleteUser0": "api" / "v1" / "user" / "profile" / "delete-account" / noArgs
  , "UpdateThread0": "api" / "v1" / "user" / "thread" / "update" / noArgs
  , "GetThreadByID0": "api" / "v1" / "thread" / (int segment)
  , "Comments" : "api" / "v1" / "thread" / "comment" / (int segment)
  , "Community" : "api" / "v1" / "community" / noArgs
  }

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete (Maybe Json)

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

data MyRoute
  = Home Pagination
  | Login
  | Register
  | OTP Int
  | CreateThread
  | ChangePassword
  | DeleteUser
  | UpdateThread Int
  | ViewThread Int

derive instance genericRoute :: Generic MyRoute _
derive instance eqRoute :: Eq MyRoute

newtype Token = Token String

derive instance eqToken :: Eq Token
instance showToken :: Show Token where
  show (Token _) = "TOKEN"

type Pagination = {
  limit :: Maybe Int
  , offset :: Maybe Int
}

myRoute :: RouteDuplex' MyRoute
myRoute = root $ G.sum
  { "Home": "Home" ? { 
        offset: optional <<< int
      , limit: optional <<< int
      }
  , "Login": path "login" G.noArgs
  , "Register": path "register" G.noArgs
  , "OTP": "otp" / (int segment)
  , "CreateThread": path "create-thread" G.noArgs
  , "ChangePassword": path "change-password" G.noArgs
  , "DeleteUser": path "delete-my-account" G.noArgs
  , "UpdateThread": "update-thread" / (int segment)
  , "ViewThread" : "view-thread" / (int segment)
  }

type PaginatedArray a =
  { total :: Int
  , body :: Array a
  }

-- Types for Fetching and inserting

type ThreadRep row =
  ( title :: String
  , description :: Maybe String
  , communityIDForThreadInfo :: Int
  , userIDForThreadInfo :: Int
  , communityNameForThreadInfo :: String
  , createdAtForThreadInfo :: String
  , downvoteCount :: Maybe Int
  , threadIDForThreadInfo :: Int
  , upvoteCount :: Maybe Int
  , userNameForThreadInfo :: String
  , commentCount :: Maybe Int
  | row
  )

type Thread = { | ThreadRep () }

type ThreadInfo = { | ThreadRep (age :: String) }

type Profile =
  { userID :: Int
  , userName :: String
  }

type OtpFields =
  { otp :: Int
  , userID :: Int
  }

type LoginFields =
  { emailForLogin :: String
  , passwordForLogin :: String
  }

type RegisterFields =
  { userNameForRegister :: String
  , emailForRegister :: String
  , passwordForRegister :: String
  , confirmPasswordForRegister :: String
  }

type CreateThreadFields =
  { threadTitleForCreate :: String
  , threadDescriptionForCreate :: String
  , threadCommunityIDForCreate :: Int
  }

type ChangePasswordFields =
  { oldPasswordForChangePass :: String
  , newPasswordForChangePass :: String
  , confirmPasswordForChangePass :: String
  }

type DeleteUserFields =
  { passwordForDeleteUser :: String
  , areUSure :: Boolean
  }

type UpdateThreadFields =
  { threadIDForUpdate :: Int
  , threadTitleForUpdate :: String
  , threadDescriptionForUpdate :: String
  , threadCommunityIDForUpdate :: Int
  }

type CommentInfo = {
    commentIDForCommentInfo :: Int,
    commentContentForCommentInfo :: String,
    userIDForCommentInfo :: Int,
    userNameForCommentInfo :: String,
    threadIDForCommentInfo :: Int,
    createdAtForCommentInfo :: String,
    parentCommentIDForCommentInfo :: Maybe Int
}

newtype NestedComment = NestedComment {
    mainComment :: CommentInfo
  , children :: Array NestedComment
 }

derive instance newtypeNestedComment ∷ Newtype NestedComment _

type CommunityRep row = (
  communityID :: Int,
  communityName :: String
  | row
)

type Community = { | CommunityRep () }
type CommunityInfo = { 
  | CommunityRep 
  ( communityCreatedAt :: String
  , communityDescription :: String
  --TODO: , communityLabelList 
  ) 
  }

communityCodec :: JsonCodec Community
communityCodec =
  CAR.object "comunity" {
    communityID : CA.int,
    communityName : CA.string
  }

communitiesCodec :: JsonCodec (PaginatedArray Community) 
communitiesCodec = 
    CAM.renameField "communities" "body"
        >~> CAM.renameField "communityCount" "total"
        >~> codec
  where
    codec =
        CAR.object "Paginated Communities"
            { body : CA.array communityCodec
             , total: CA.int
            }

profileCodec :: JsonCodec Profile
profileCodec =
  CAM.renameField "userIDForUPR" "userID"
    >~> CAM.renameField "userNameForUPR" "userName"
    >~>
      ( CAR.object "Profile"
          { userID: CA.int
          , userName: CA.string
          }
      )

threadCodec :: JsonCodec Thread
threadCodec =
  CAR.object "Thread"
    { threadIDForThreadInfo: CA.int
    , title: CA.string
    , description: CAC.maybe CA.string
    , userIDForThreadInfo: CA.int
    , userNameForThreadInfo: CA.string
    , communityIDForThreadInfo: CA.int
    , communityNameForThreadInfo: CA.string
    , createdAtForThreadInfo: CA.string
    , upvoteCount: CAC.maybe CA.int
    , downvoteCount: CAC.maybe CA.int
    , commentCount: CAC.maybe CA.int
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
    , emailForRegister: CA.string
    , passwordForRegister: CA.string
    , confirmPasswordForRegister: CA.string
    }

createThreadCodec :: JsonCodec CreateThreadFields
createThreadCodec =
  CAR.object "create thread fields"
    { threadTitleForCreate: CA.string
    , threadDescriptionForCreate: CA.string
    , threadCommunityIDForCreate: CA.int
    }

changePasswordCodec :: JsonCodec ChangePasswordFields
changePasswordCodec =
  CAR.object "change password fields"
    { oldPasswordForChangePass: CA.string
    , newPasswordForChangePass: CA.string
    , confirmPasswordForChangePass: CA.string
    }

deleteUserCodec :: JsonCodec DeleteUserFields
deleteUserCodec =
  CAR.object "Delete user"
    { passwordForDeleteUser: CA.string
    , areUSure: CA.boolean
    }

updateThreadCodec :: JsonCodec UpdateThreadFields
updateThreadCodec =
  CAR.object "update thread"
    { threadIDForUpdate: CA.int
    , threadTitleForUpdate: CA.string
    , threadDescriptionForUpdate: CA.string
    , threadCommunityIDForUpdate: CA.int
    }

commentCodec :: JsonCodec CommentInfo
commentCodec = 
    CAR.object "commmentInfo" {
        commentIDForCommentInfo : CA.int,
        commentContentForCommentInfo : CA.string,
        userIDForCommentInfo : CA.int,
        userNameForCommentInfo : CA.string,
        threadIDForCommentInfo : CA.int,
        createdAtForCommentInfo : CA.string,
        parentCommentIDForCommentInfo : CAC.maybe CA.int
    }

nestedCommentCodec :: JsonCodec NestedComment
nestedCommentCodec =
    CA.fix \e ->
      wrapIso NestedComment $
        CAR.object "NestedComment" {
        mainComment : commentCodec
            , children : CA.array e
          }

nestedCommentsCodec :: JsonCodec (PaginatedArray NestedComment) 
nestedCommentsCodec = 
    CAM.renameField "comments" "body"
        >~> CAM.renameField "commentsCount" "total"
        >~> codec
  where
    codec =
        CAR.object "Paginated NestedComment"
            { body : CA.array nestedCommentCodec
             , total: CA.int
            }
