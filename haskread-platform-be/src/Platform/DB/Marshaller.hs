{-# LANGUAGE RecordWildCards #-}

module Platform.DB.Marshaller
  ( userIDField,
    userNameField,
    emailField,
    passwordField,
    createdAtField,
    updatedAtField,
    userMarshaller,
    userProfileImageMarshaller,
    userImageField,
    adminMarshaller,
    adminIDField,
    communityMarshaller,
    communityIDField,
    communityNameField,
    threadMarshaller,
    threadIDField,
    threadVoteMarshaller,
    commentMarshaller,
    commentIDField,
    commentVoteMarshaller,
    userEmailVerifyOTPMarshaller,
    threadInfoMarshaller,
    threadTitleField,
    threadDescriptionField,
    upvoteCountField,
    downvoteCountField,
    voteField,
    commentCountField,
    commentInfoMarshaller,
    commentContentField,
    parentCommentIDField,
    threadAttachmentField,
    threadAttachmentNameField,
    threadAttachmentSizeField,
  )
where

import Data.Password.Bcrypt
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Int (Int32)
import Orville.PostgreSQL
import Platform.Common.Types (MyPassword (..))
import Platform.DB.Model

otpField :: FieldDefinition NotNull Int32
otpField = integerField "otp"

threadIDField :: FieldDefinition NotNull ThreadID
threadIDField = coerceField $ serialField "thread_id"

adminNameField :: FieldDefinition NotNull Text
adminNameField = boundedTextField "admin_name" 255

adminIDField :: FieldDefinition NotNull AdminID
adminIDField = coerceField $ serialField "admin_id"

userIDField :: FieldDefinition NotNull UserID
userIDField = coerceField $ serialField "user_id"

userNameField :: FieldDefinition NotNull Text
userNameField = boundedTextField "user_name" 255

emailField :: FieldDefinition NotNull Text
emailField = boundedTextField "email" 255

passwordField :: FieldDefinition NotNull MyPassword
passwordField = coerceField (unboundedTextField "password")

communityIDField :: FieldDefinition NotNull CommunityID
communityIDField = coerceField $ serialField "community_id"

threadAttachmentField :: FieldDefinition Nullable (Maybe Text)
threadAttachmentField =  nullableField $ unboundedTextField "attachment"

communityNameField :: FieldDefinition NotNull Text
communityNameField = boundedTextField "community_name" 255

communityDescriptionField :: FieldDefinition NotNull Text
communityDescriptionField = unboundedTextField "community_description"

communityLabelListField :: FieldDefinition NotNull Text
communityLabelListField = unboundedTextField "label_list"

upvoteCountField :: FieldDefinition NotNull Int32
upvoteCountField = integerField "upvote_count"

downvoteCountField :: FieldDefinition NotNull Int32
downvoteCountField = integerField "downvote_count"

commentCountField :: FieldDefinition NotNull Int32
commentCountField = integerField "comment_count"

createdAtField :: FieldDefinition NotNull UTCTime
createdAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "created_at"

updatedAtField :: FieldDefinition NotNull UTCTime
updatedAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "updated_at"

userImageField :: FieldDefinition NotNull Text
userImageField = unboundedTextField "user_profile_image"

threadTitleField :: FieldDefinition NotNull Text
threadTitleField = boundedTextField "thread_title" 255

threadDescriptionField :: FieldDefinition NotNull Text
threadDescriptionField = unboundedTextField "thread_description"

voteField :: FieldDefinition NotNull Bool
voteField = booleanField "vote"

commentIDField :: FieldDefinition NotNull CommentID
commentIDField = coerceField $ serialField "comment_id"

commentContentField :: FieldDefinition NotNull Text
commentContentField = boundedTextField "comment_content" 255

parentCommentIDField :: FieldDefinition Nullable (Maybe CommentID)
parentCommentIDField = nullableField $ coerceField $ integerField "parent_comment_id"

isVerifiedField :: FieldDefinition NotNull Bool
isVerifiedField = setDefaultValue (booleanDefault False) $ booleanField "is_verified"

userMarshaller :: SqlMarshaller UserWrite UserRead
userMarshaller =
  User
    <$> marshallReadOnly
      ( marshallField
          (\User {..} -> userID)
          userIDField
      )
    <*> marshallField (\User {..} -> userName) userNameField
    <*> marshallField (\User {..} -> email) emailField
    <*> marshallField (\User {..} -> userPassword) (nullableField passwordField)
    <*> marshallField (\User {..} -> isUserVerified) isVerifiedField
    <*> marshallReadOnly
      ( marshallField
          (\User {..} -> createdAt)
          createdAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\User {..} -> updatedAt)
          updatedAtField
      )

userProfileImageMarshaller ::
  SqlMarshaller
    UserProfileImageWrite
    UserProfileImageRead
userProfileImageMarshaller =
  UserProfileImage
    <$> marshallField
      (\UserProfileImage {..} -> userIDForProfileImage)
      userIDField
    <*> marshallField (\UserProfileImage {..} -> userImage) userImageField
    <*> marshallReadOnly
      ( marshallField
          (\UserProfileImage {..} -> createdAtForProfileImage)
          createdAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\UserProfileImage {..} -> updatedAtForProfileImage)
          updatedAtField
      )

adminMarshaller ::
  SqlMarshaller
    AdminWrite
    AdminRead
adminMarshaller =
  Admin
    <$> marshallReadOnly
      ( marshallField
          (\Admin {..} -> adminID)
          adminIDField
      )
    <*> marshallField (\Admin {..} -> adminName) adminNameField
    <*> marshallField (\Admin {..} -> adminEmail) emailField
    <*> marshallField (\Admin {..} -> adminPassword) passwordField
    <*> marshallReadOnly (marshallField (\Admin {..} -> createdAtForAdmin) createdAtField)
    <*> marshallReadOnly (marshallField (\Admin {..} -> updatedAtForAdmin) updatedAtField)

communityMarshaller ::
  SqlMarshaller
    CommunityWrite
    CommunityRead
communityMarshaller =
  Community
    <$> marshallReadOnly
      ( marshallField
          (\Community {..} -> communityID)
          communityIDField
      )
    <*> marshallField (\Community {..} -> communityName) communityNameField
    <*> marshallField (\Community {..} -> communityDescription) communityDescriptionField
    <*> marshallField (\Community {..} -> communityLabelList) communityLabelListField
    <*> marshallReadOnly (marshallField (\Community {..} -> communityCreatedAt) createdAtField)
    <*> marshallReadOnly (marshallField (\Community {..} -> communityUpdatedAt) updatedAtField)

threadMarshaller ::
  SqlMarshaller
    ThreadWrite
    ThreadRead
threadMarshaller =
  Thread
    <$> marshallReadOnly
      ( marshallField
          (\Thread {..} -> threadID)
          threadIDField
      )
    <*> marshallField (\Thread {..} -> threadTitle) threadTitleField
    <*> marshallField (\Thread {..} -> threadDescription) (nullableField threadDescriptionField)
    <*> marshallField (\Thread {..} -> threadUserID) userIDField
    <*> marshallField (\Thread {..} -> threadCommunityID) communityIDField
    <*> marshallField (\Thread {..} -> threadAttachment) threadAttachmentField
    <*> marshallField (\Thread {..} -> threadAttachmentSize) threadAttachmentSizeField
    <*> marshallField (\Thread {..} -> threadAttachmentName) threadAttachmentNameField
    <*> marshallReadOnly (marshallField (\Thread {..} -> threadCreatedAt) createdAtField)
    <*> marshallReadOnly (marshallField (\Thread {..} -> threadUpdatedAt) updatedAtField)

threadVoteMarshaller ::
  SqlMarshaller
    ThreadVoteWrite
    ThreadVoteRead
threadVoteMarshaller =
  ThreadVote
    <$> marshallField (\ThreadVote {..} -> threadVoteUserID) userIDField
    <*> marshallField (\ThreadVote {..} -> threadVoteThreadID) threadIDField
    <*> marshallField (\ThreadVote {..} -> threadVote) voteField
    <*> marshallReadOnly (marshallField (\ThreadVote {..} -> threadVoteCreatedAt) createdAtField)
    <*> marshallReadOnly (marshallField (\ThreadVote {..} -> threadVoteUpdatedAt) updatedAtField)

commentMarshaller ::
  SqlMarshaller
    CommentWrite
    CommentRead
commentMarshaller =
  Comment
    <$> marshallReadOnly
      ( marshallField
          (\Comment {..} -> commentID)
          commentIDField
      )
    <*> marshallField (\Comment {..} -> userIDForComment) userIDField
    <*> marshallField (\Comment {..} -> threadIDForComment) threadIDField
    <*> marshallField (\Comment {..} -> commentContent) commentContentField
    <*> marshallField (\Comment {..} -> parentCommentID) parentCommentIDField
    <*> marshallReadOnly (marshallField (\Comment {..} -> createdAtForComment) createdAtField)
    <*> marshallReadOnly (marshallField (\Comment {..} -> updatedAtForComment) updatedAtField)

commentInfoMarshaller ::
  SqlMarshaller
    CommentInfo
    CommentInfo
commentInfoMarshaller =
  CommentInfo
    <$> marshallField (\CommentInfo {..} -> commentIDForCommentInfo) commentIDField
    <*> marshallField (\CommentInfo {..} -> commentContentForCommentInfo) commentContentField
    <*> marshallField (\CommentInfo {..} -> userIDForCommentInfo) userIDField
    <*> marshallField (\CommentInfo {..} -> userNameForCommentInfo) userNameField
    <*> marshallField (\CommentInfo {..} -> threadIDForCommentInfo) threadIDField
    <*> marshallField (\CommentInfo {..} -> createdAtForCommentInfo) createdAtField
    <*> marshallField (\CommentInfo {..} -> parentCommentIDForCommentInfo) parentCommentIDField
    <*> marshallField (\CommentInfo {..} -> commentUpvoteCount) (nullableField upvoteCountField)
    <*> marshallField (\CommentInfo {..} -> commentDownvoteCount) (nullableField downvoteCountField)

-- -- CommentVote Model
commentVoteMarshaller ::
  SqlMarshaller
    CommentVoteWrite
    CommentVoteRead
commentVoteMarshaller =
  CommentVote
    <$> marshallField (\CommentVote {..} -> userIDForCommentVote) userIDField
    <*> marshallField (\CommentVote {..} -> commentIDForCommentVote) commentIDField
    <*> marshallField (\CommentVote {..} -> commentVote) voteField
    <*> marshallReadOnly
      ( marshallField
          (\CommentVote {..} -> createdAtForCommentVote)
          createdAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\CommentVote {..} -> updatedAtForCommentVote)
          updatedAtField
      )

-- UserEmailVerifyOTP Model

userEmailVerifyOTPMarshaller ::
  SqlMarshaller
    UserEmailVerifyOTPWrite
    UserEmailVerifyOTPRead
userEmailVerifyOTPMarshaller =
  UserEmailVerifyOTP
    <$> marshallField (\UserEmailVerifyOTP {..} -> userIDForUEVO) userIDField
    <*> marshallField (\UserEmailVerifyOTP {..} -> otpForUEVO) otpField
    <*> marshallReadOnly
      ( marshallField
          (\UserEmailVerifyOTP {..} -> createdAtForUEVO)
          createdAtField
      )

-- Custom marshallers for fetching
threadInfoMarshaller ::
  SqlMarshaller
    ThreadInfo
    ThreadInfo
threadInfoMarshaller =
  ThreadInfo
    <$> marshallField (\ThreadInfo {..} -> threadIDForThreadInfo) threadIDField
    <*> marshallField (\ThreadInfo {..} -> title) threadTitleField
    <*> marshallField
      (\ThreadInfo {..} -> description)
      (nullableField threadDescriptionField)
    <*> marshallField (\ThreadInfo {..} -> createdAtForThreadInfo) createdAtField
    <*> marshallField (\ThreadInfo {..} -> attachmentSize) threadAttachmentSizeField
    <*> marshallField (\ThreadInfo {..} -> attachmentName) threadAttachmentNameField
    <*> marshallField (\ThreadInfo {..} -> userIDForThreadInfo) userIDField
    <*> marshallField (\ThreadInfo {..} -> userNameForThreadInfo) userNameField
    <*> marshallField (\ThreadInfo {..} -> communityIDForThreadInfo) communityIDField
    <*> marshallField (\ThreadInfo {..} -> communityNameForThreadInfo) communityNameField
    <*> marshallField
      (\ThreadInfo {..} -> upvoteCount)
      (nullableField upvoteCountField)
    <*> marshallField
      (\ThreadInfo {..} -> downvoteCount)
      (nullableField downvoteCountField)
    <*> marshallField
      (\ThreadInfo {..} -> commentCount)
      (nullableField commentCountField)

threadAttachmentSizeField :: FieldDefinition Nullable (Maybe Int32)
threadAttachmentSizeField = nullableField $ integerField "attachment_size"

threadAttachmentNameField :: FieldDefinition Nullable (Maybe Text)
threadAttachmentNameField = nullableField $ boundedTextField "attachment_name" 500
