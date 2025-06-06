{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TestApp.SampleData
  ( sampleAdmins,
    sampleAdminInfoList,
    sampleUsers,
    sampleUserInfoList,
    sampleComments,
    sampleCommunities,
    sampleThreads,
    commentUpdateReqBody,
    commentCreateReqBody,
    registerUserBody,
    correctLoginUserBody,
    sampleChangePasswordBody,
    sampleDeleteUserBody,
    loginAdminBody,
    adminChangePasswordBody,
    adminCreateAdminReqBody,
    communityCreateReqBody,
    communityUpdateReqBody,
    sampleUpdateUserProfileImageBody,
    threadCreateReqBody,
    threadUpdateReqBody,
    sampleUEVO,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Password.Bcrypt
import Platform.Admin.Types hiding (AdminDashboardResponse (..))
import Platform.Auth.Types
import Platform.Comment.Types
import Platform.Common.Types (MyPassword (..))
import Platform.Community.Types
import Platform.DB.Model
import Platform.User.Thread.Types
import Platform.User.Types
import System.IO.Unsafe
import Servant.Multipart

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

samplePassword :: MyPassword
samplePassword =
  unsafePerformIO $
    MyPassword
      <$> (hashPassword $ mkPassword "Foobar123")

sampleAdmins :: [AdminWrite]
sampleAdmins =
  [ Admin
      { adminName = "batman",
        adminEmail = "bruce@abc.com",
        adminPassword = samplePassword,
        createdAtForAdmin = (),
        updatedAtForAdmin = (),
        adminID = ()
      },
    Admin
      { adminName = "superman",
        adminEmail = "clark@abc.com",
        adminPassword = samplePassword,
        createdAtForAdmin = (),
        updatedAtForAdmin = (),
        adminID = ()
      }
  ]

sampleUEVO :: [UserEmailVerifyOTPWrite]
sampleUEVO =
  [ UserEmailVerifyOTP
      { userIDForUEVO = UserID 2,
        otpForUEVO = 1234,
        createdAtForUEVO = ()
      },
    UserEmailVerifyOTP
      { userIDForUEVO = UserID 1,
        otpForUEVO = 1234,
        createdAtForUEVO = ()
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
        userPassword = Just samplePassword,
        isUserVerified = False,
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "spiderman",
        email = "peter@abc.com",
        userPassword = Just samplePassword,
        isUserVerified = False,
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "superman",
        email = "clark@abc.com",
        userPassword = Just samplePassword,
        isUserVerified = False,
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "wonderwoman",
        email = "diana@abc.com",
        userPassword = Just samplePassword,
        isUserVerified = False,
        userID = (),
        createdAt = (),
        updatedAt = ()
      },
    User
      { userName = "ironman",
        email = "tony@abc.com",
        userPassword = Just samplePassword,
        isUserVerified = True, -- For login test
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
sampleCommunities =
  [ Community
      { communityID = (),
        communityName = "haskell",
        communityDescription = "functional programming language",
        communityLabelList = "[\"monads\",\"functors\"]",
        communityCreatedAt = (),
        communityUpdatedAt = ()
      },
    Community
      { communityID = (),
        communityName = "python",
        communityDescription = "general purpose programming language",
        communityLabelList = "[\"django\",\"flask\"]",
        communityCreatedAt = (),
        communityUpdatedAt = ()
      },
    Community
      { communityID = (),
        communityName = "java",
        communityDescription = "general purpose programming language",
        communityLabelList = "[\"spring\",\"hibernate\"]",
        communityCreatedAt = (),
        communityUpdatedAt = ()
      },
    Community
      { communityID = (),
        communityName = "javascript",
        communityDescription = "general purpose programming language",
        communityLabelList = "[\"react\",\"angular\"]",
        communityCreatedAt = (),
        communityUpdatedAt = ()
      },
    Community
      { communityID = (),
        communityName = "ruby",
        communityDescription = "general purpose programming language",
        communityLabelList = "[\"rails\",\"sinatra\"]",
        communityCreatedAt = (),
        communityUpdatedAt = ()
      }
  ]

sampleThreads :: [ThreadWrite]
sampleThreads =
  [ Thread
      { threadID = (),
        threadTitle = "HaskRead",
        threadDescription = Just "A Reddit Clone in Haskell",
        threadCommunityID = CommunityID 1,
        threadUserID = UserID 1,
        threadAttachment = Nothing,
        threadAttachmentSize = Nothing,
        threadAttachmentName = Nothing,
        threadCreatedAt = (),
        threadUpdatedAt = ()
      },
    Thread
      { threadID = (),
        threadTitle = "HaskRead 2",
        threadDescription = Just "A Reddit Clone in Haskell updated.",
        threadCommunityID = CommunityID 1,
        threadUserID = UserID 1,
        threadAttachment = Nothing,
        threadAttachmentSize = Nothing,
        threadAttachmentName = Nothing,
        threadCreatedAt = (),
        threadUpdatedAt = ()
      },
    Thread
      { threadID = (),
        threadTitle = "HaskRead 3",
        threadDescription = Just "A Reddit Clone in Haskell updated again.",
        threadCommunityID = CommunityID 2,
        threadUserID = UserID 1,
        threadAttachment = Nothing,
        threadAttachmentSize = Nothing,
        threadAttachmentName = Nothing,
        threadCreatedAt = (),
        threadUpdatedAt = ()
      },
    Thread
      { threadID = (),
        threadTitle = "HaskRead 4",
        threadDescription = Just "A Reddit Clone in Haskell updated again.",
        threadCommunityID = CommunityID 2,
        threadUserID = UserID 1,
        threadAttachment = Nothing,
        threadAttachmentSize = Nothing,
        threadAttachmentName = Nothing,
        threadCreatedAt = (),
        threadUpdatedAt = ()
      }
  ]

sampleComments :: [CommentWrite]
sampleComments =
  [ Comment
      { commentID = (),
        commentContent = "Great work!",
        threadIDForComment = ThreadID 3,
        userIDForComment = UserID 1,
        createdAtForComment = (),
        updatedAtForComment = (),
        parentCommentID = Nothing
      },
    Comment
      { commentID = (),
        commentContent = "Great work! Updated.",
        threadIDForComment = ThreadID 3,
        userIDForComment = UserID 1,
        createdAtForComment = (),
        updatedAtForComment = (),
        parentCommentID = Just $ CommentID 1
      },
    Comment
      { commentID = (),
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
    UpdateCommentReqBody
      { commentContentForUpdate = "Great work! Updated."
      }

commentCreateReqBody :: ByteString
commentCreateReqBody =
  encode
    CreateCommentReqBody
      { commentContentForCreate = "Great work!",
        threadIDForCommentCreate = ThreadID 2,
        parentCommentIDForCreate = Nothing
      }

registerUserBody :: ByteString
registerUserBody =
  let r =
        RegisterUserBody
          { userNameForRegister = "tushar",
            emailForRegister = "ratushar46@gmail.com",
            passwordForRegister = "Tushar123",
            confirmPasswordForRegister = "Tushar123"
          }
   in encode r

correctLoginUserBody :: ByteString
correctLoginUserBody =
  let l =
        LoginUserBody
          { emailForLogin = "tony@abc.com",
            passwordForLogin = "Foobar123"
          }
   in encode l

sampleChangePasswordBody :: ByteString
sampleChangePasswordBody =
  encode
    ChangePasswordBody
      { oldPasswordForChangePass = "Foobar123",
        newPasswordForChangePass = "Foobar1234",
        confirmPasswordForChangePass = "Foobar1234"
      }

sampleDeleteUserBody :: ByteString
sampleDeleteUserBody =
  encode
    DeleteUserBody
      { passwordForDeleteUser = "Foobar123",
        areUSure = True
      }

loginAdminBody :: ByteString
loginAdminBody =
  encode
    AdminLoginBodyReq
      { adminEmailForLogin = "bruce@abc.com",
        adminPasswordForLogin = "Foobar123"
      }

adminChangePasswordBody :: ByteString
adminChangePasswordBody =
  encode
    AdminChangePasswordBody
      { oldPassword = "Foobar123",
        newPassword = "Foobar1234",
        confirmNewPassword = "Foobar1234"
      }

adminCreateAdminReqBody :: ByteString
adminCreateAdminReqBody =
  encode
    AdminCreateAdminReqBody
      { adminNameForCreate = "deadpool",
        adminEmailForCreate = "wade@abc.com",
        adminPasswordForCreate = "Foobar123",
        adminConfirmPasswordForCreate = "Foobar123"
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
  BSL.readFile "./test/_sampleData/random_image.png"

threadCreateReqBody :: BSL.ByteString
threadCreateReqBody = 
    let boundary = "------------------------boundary123456789"
        addField name value = 
            "--" <> boundary <> "\r\n" <>
            "Content-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <>
            value <> "\r\n"
    in
    addField "threadTitleForCreate" "Test Thread Title" <>
    addField "threadCommunityIDForCreate" "1" <>
    addField "threadDescriptionForCreate" "This is a test thread description." <>
    "--" <> boundary <> "--\r\n"

threadUpdateReqBody :: ByteString
threadUpdateReqBody =
  let boundary = "------------------------boundary123456789"
      contentType = "multipart/form-data; boundary=" <> boundary
      addField name value = 
            "--" <> boundary <> "\r\n" <>
            "Content-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <>
            value <> "\r\n"
    in
    addField "threadIDForUpdate" "2" <>
    addField "threadTitleForUpdate" "Test Thread Title 2" <>
    addField "threadCommunityIDForUpdate" "1" <>
    addField "threadDescriptionForUpdate" "This is a test thread description." <>
    "--" <> boundary <> "--\r\n"
