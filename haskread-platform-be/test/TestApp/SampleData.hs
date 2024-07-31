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

sampleAdmins :: [AdminWrite]
sampleAdmins =
  [ Admin
      { adminName = "batman",
        adminEmail = "bruce@abc.com",
        adminPassword = "1234",
        createdAtForAdmin = (),
        updatedAtForAdmin = (),
        adminID = ()
      }
  ]

sampleAdminInfoList :: [AdminInfo]
sampleAdminInfoList =
  [AdminInfo (AdminID 1) "batman"]

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
  }
  ]

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
          { emailForLogin = "peter@abc.com",
            passwordForLogin = "Peter123"
          }
   in encode l

sampleChangePasswordBody :: ByteString
sampleChangePasswordBody =
  encode
    ChangePasswordBody
      { oldPasswordForChangePass = "Bruce123",
        newPasswordForChangePass = "Bruce1235",
        confirmPasswordForChangePass = "Bruce1235"
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
        adminPasswordForLogin = "1234"
      }

adminChangePasswordBody :: ByteString
adminChangePasswordBody =
  encode
    AdminChangePasswordBody
      { oldPassword = "1234",
        newPassword = "1235",
        confirmNewPassword = "1235"
      }

adminCreateAdminReqBody :: ByteString
adminCreateAdminReqBody =
  encode
    AdminCreateAdminReqBody
      { adminNameForCreate = "wade",
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
  }
 ]