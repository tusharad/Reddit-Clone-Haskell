{-# LANGUAGE OverloadedStrings #-}

module TestApp.SampleData where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Platform.Admin.Community.Types
import Platform.Admin.Types hiding (AdminDashboardResponse (..))
import Platform.Auth.Types
import Platform.User.Thread.Types
import Platform.DB.Model
import Platform.User.Types
import Platform.Comment.Types

{-
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genAdmin :: Gen AdminWrite
genAdmin =
  Admin () 
    <$> Gen.text (Range.linear 10 19) Gen.alpha
    <*> Gen.text (Range.linear 10 19) Gen.alpha
    <*> Gen.text (Range.linear 10 19) Gen.alpha
    <*> pure ()
    <*> pure ()
-}

sampleAdmins :: [AdminWrite]
sampleAdmins =
  [ Admin
      { adminName = "batman",
        adminEmail = "bruce@abc.com",
        adminPassword = "Bruce123",
        createdAtForAdmin = (),
        updatedAtForAdmin = (),
        adminID = ()
      },
    Admin {
      adminName = "superman",
      adminEmail = "clark@abc.com",
      adminPassword = "Clark123",
      createdAtForAdmin = (),
      updatedAtForAdmin = (),
      adminID = ()
    }
  ]

sampleAdminInfoList :: [AdminInfo]
sampleAdminInfoList =
  [ AdminInfo (AdminID 1) "batman",
    AdminInfo (AdminID 2) "superman"
  ]

sampleUsers :: [UserWrite]
sampleUsers =
  [ User
      { userName = "batman",
        email = "bruce@abc.com",
        password = "Bruce123",
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "spiderman",
        email = "peter@abc.com",
        password = "Peter123",
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "superman",
        email = "clark@abc.com",
        password = "Clark123",
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "wonderwoman",
        email = "diana@abc.com",
        password = "Diana123",
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "ironman",
        email = "tony@abc.com",
        password = "Tony123",
        userID = (),
        createdAt = (),
        updatedAt = ()
      }
  ]

sampleUserInfoList :: [UserInfo]
sampleUserInfoList =
  [ UserInfo
      { userIDForUserInfo = UserID 1,
        userNameForUserInfo = "batman"
      },
    UserInfo
      { userIDForUserInfo = UserID 2,
        userNameForUserInfo = "spiderman"
      },
    UserInfo
      { userIDForUserInfo = UserID 3,
        userNameForUserInfo = "superman"
      },
    UserInfo
      { userIDForUserInfo = UserID 4,
        userNameForUserInfo = "wonderwoman"
      },
    UserInfo
      { userIDForUserInfo = UserID 5,
        userNameForUserInfo = "ironman"
      }
  ]

sampleCommunities :: [CommunityWrite]
sampleCommunities = [
   Community {
    communityID = (),
    communityName = "haskell",
    communityDescription = "functional programming language",
    communityLabelList = "[\"monads\",\"functors\"]",
    communityCreatedAt = (),
    communityUpdatedAt = ()
  },
  Community {
    communityID = (),
    communityName = "python",
    communityDescription = "general purpose programming language",
    communityLabelList = "[\"django\",\"flask\"]",
    communityCreatedAt = (),
    communityUpdatedAt = ()
  },
  Community {
    communityID = (),
    communityName = "java",
    communityDescription = "general purpose programming language",
    communityLabelList = "[\"spring\",\"hibernate\"]",
    communityCreatedAt = (),
    communityUpdatedAt = ()
  },
  Community {
    communityID = (),
    communityName = "javascript",
    communityDescription = "general purpose programming language",
    communityLabelList = "[\"react\",\"angular\"]",
    communityCreatedAt = (),
    communityUpdatedAt = ()
  },
  Community {
    communityID = (),
    communityName = "ruby",
    communityDescription = "general purpose programming language",
    communityLabelList = "[\"rails\",\"sinatra\"]",
    communityCreatedAt = (),
    communityUpdatedAt = ()
  }
  ]

sampleThreads :: [ThreadWrite]
sampleThreads = [
  Thread {
    threadID = (),
    threadTitle = "HaskRead",
    threadDescription = Just "A Reddit Clone in Haskell",
    threadCommunityID = CommunityID 1,
    threadUserID = UserID 1,
    threadCreatedAt = (),
    threadUpdatedAt = ()
  },
  Thread {
    threadID = (),
    threadTitle = "HaskRead 2",
    threadDescription = Just "A Reddit Clone in Haskell updated.",
    threadCommunityID = CommunityID 1,
    threadUserID = UserID 1,
    threadCreatedAt = (),
    threadUpdatedAt = ()
  },
  Thread {
    threadID = (),
    threadTitle = "HaskRead 3",
    threadDescription = Just "A Reddit Clone in Haskell updated again.",
    threadCommunityID = CommunityID 2,
    threadUserID = UserID 1,
    threadCreatedAt = (),
    threadUpdatedAt = ()
  },
  Thread {
    threadID = (),
    threadTitle = "HaskRead 4",
    threadDescription = Just "A Reddit Clone in Haskell updated again.",
    threadCommunityID = CommunityID 2,
    threadUserID = UserID 1,
    threadCreatedAt = (),
    threadUpdatedAt = ()
  }
 ]

sampleComments :: [CommentWrite]
sampleComments = [
  Comment {
    commentID = (),
    commentContent = "Great work!",
    threadIDForComment = ThreadID 3,
    userIDForComment = UserID 1,
    createdAtForComment = (),
    updatedAtForComment = (),
    parentCommentID = Nothing
  },
  Comment {
    commentID = (),
    commentContent = "Great work! Updated.",
    threadIDForComment = ThreadID 3,
    userIDForComment = UserID 1,
    createdAtForComment = (),
    updatedAtForComment = (),
    parentCommentID = Just $ CommentID 1
  },
  Comment {
    commentID = (),
    commentContent = "Great work again!",
    threadIDForComment = ThreadID 2,
    userIDForComment = UserID 1,
    createdAtForComment = (),
    updatedAtForComment = (),
    parentCommentID = Nothing
  }
 ]

commentUpdateReqBody :: ByteString
commentUpdateReqBody =
  encode
    UpdateCommentReqBody {
      commentContentForUpdate = "Great work! Updated."
    }

commentCreateReqBody :: ByteString
commentCreateReqBody =
  encode
    CreateCommentReqBody {
      commentContentForCreate = "Great work!",
      threadIDForCommentCreate = ThreadID 2
    }

registerUserBody :: ByteString
registerUserBody =
  let r =
        RegisterUserBody
          { userNameForRegister = "tushar",
            emailForRegister = "tushar@abc",
            passwordForRegister = "Tushar123",
            confirmPasswordForRegister = "Tushar123"
          }
   in encode r

correctLoginUserBody :: ByteString
correctLoginUserBody =
  let l =
        LoginUserBody
          { emailForLogin = "tony@abc.com",
            passwordForLogin = "Tony123"
          }
   in encode l

sampleChangePasswordBody :: ByteString
sampleChangePasswordBody =
  encode
    ChangePasswordBody
      { oldPasswordForChangePass = "Peter123",
        newPasswordForChangePass = "Peter1234",
        confirmPasswordForChangePass = "Peter1234"
      }

sampleDeleteUserBody :: ByteString
sampleDeleteUserBody =
  encode
    DeleteUserBody
      { passwordForDeleteUser = "Clark123",
        areUSure = True
      }

loginAdminBody :: ByteString
loginAdminBody =
  encode
    AdminLoginBodyReq
      { adminEmailForLogin = "bruce@abc.com",
        adminPasswordForLogin = "Bruce123"
      }

adminChangePasswordBody :: ByteString
adminChangePasswordBody =
  encode
    AdminChangePasswordBody
      { oldPassword = "Clark123",
        newPassword = "Clark1234",
        confirmNewPassword = "Clark1234"
      }

adminCreateAdminReqBody :: ByteString
adminCreateAdminReqBody =
  encode
    AdminCreateAdminReqBody
      { adminNameForCreate = "deadpool",
        adminEmailForCreate = "wade@abc.com",
        adminPasswordForCreate = "Wade1234",
        adminConfirmPasswordForCreate = "Wade1234"
      }

communityCreateReqBody :: ByteString
communityCreateReqBody =
  encode
    CommunityCreateReqBody
      { communityNameForCreate = "programming",
        communityDescriptionForCreate = "Community for programming.",
        communityLabelListForCreate = ["python", "java"]
      }

communityUpdateReqBody :: ByteString
communityUpdateReqBody =
  encode
    CommunityUpdateReqBody
      { communityIDForUpdate = CommunityID 1,
        communityNameForUpdate = "programming 2",
        communityDescriptionForUpdate = "Community for programming updated.",
        communityLabelListForUpdate = ["python", "java"]
      }

sampleUpdateUserProfileImageBody :: IO ByteString
sampleUpdateUserProfileImageBody =
  BSL.readFile "/home/user/haskell/Reddit-Clone-Haskell/haskread-platform-be/test/_sampleData/random_image.png"

threadCreateReqBody :: ByteString
threadCreateReqBody =
  encode
    CreateThreadReqBody {
      threadTitleForCreate = "HaskRead",
      threadDescriptionForCreate = Just "A Reddit Clone in Haskell",
      threadCommunityIDForCreate = CommunityID 1
    }

threadUpdateReqBody :: ByteString
threadUpdateReqBody =
  encode
    UpdateThreadReqBody {
      threadIDForUpdate = ThreadID 2,
      threadTitleForUpdate = "HaskRead 2",
      threadDescriptionForUpdate = Just "A Reddit Clone in Haskell updated.",
      threadCommunityIDForUpdate = CommunityID 1
    }