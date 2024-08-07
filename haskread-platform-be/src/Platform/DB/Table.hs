{-# LANGUAGE RecordWildCards #-}

module Platform.DB.Table
  ( userTable,
    userProfileImageTable,
    adminTable,
    communityTable,
    threadTable,
    threadVoteTable,
    commentTable,
    commentVoteTable,
    userEmailVerifyOTPTable,
  )
where

import Data.List.NonEmpty
import Orville.PostgreSQL
import Platform.DB.Marshaller
import Platform.DB.Model

userTable :: TableDefinition (HasKey UserID) UserWrite UserRead
userTable =
  addTableConstraints
    [ uniqueConstraint
        ( fieldName emailField
            :| [fieldName userNameField]
        )
    ]
    ( mkTableDefinition
        "users"
        (primaryKey userIDField)
        userMarshaller
    )

userProfileImageTable ::
  TableDefinition
    (HasKey UserID)
    UserProfileImageWrite
    UserProfileImageRead
userProfileImageTable =
  addTableConstraints
    [ foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName userIDField)
            (fieldName userIDField)
            :| []
        )
        (defaultForeignKeyOptions {foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})
    ]
    ( mkTableDefinition
        "user_profile_image"
        (primaryKey userIDField)
        userProfileImageMarshaller
    )

adminTable ::
  TableDefinition
    (HasKey AdminID)
    AdminWrite
    AdminRead
adminTable =
  addTableConstraints
    [ uniqueConstraint
        ( fieldName emailField
            :| []
        )
    ]
    ( mkTableDefinition
        "admin"
        (primaryKey adminIDField)
        adminMarshaller
    )

communityTable ::
  TableDefinition
    (HasKey CommunityID)
    CommunityWrite
    CommunityRead
communityTable =
  addTableConstraints
    [ uniqueConstraint
        ( fieldName communityNameField
            :| []
        )
    ]
    ( mkTableDefinition
        "community"
        (primaryKey communityIDField)
        communityMarshaller
    )

threadTable ::
  TableDefinition
    (HasKey ThreadID)
    ThreadWrite
    ThreadRead
threadTable =
  addTableConstraints
    [threadToUserForeignKeyConstraint, threadToCommunityForeignKeyConstraint]
    ( mkTableDefinition
        "thread"
        (primaryKey threadIDField)
        threadMarshaller
    )
  where
    threadToUserForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName userIDField)
            (fieldName userIDField)
            :| []
        )
        (defaultForeignKeyOptions {foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})

    threadToCommunityForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier communityTable)
        ( foreignReference
            (fieldName communityIDField)
            (fieldName communityIDField)
            :| []
        )
        (defaultForeignKeyOptions {foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})

userThreadCompositeKey :: PrimaryKey ThreadVoteID
userThreadCompositeKey =
  compositePrimaryKey
    (primaryKeyPart (\ThreadVoteID {..} -> threadVoteIDUserID) userIDField)
    [primaryKeyPart (\ThreadVoteID {..} -> threadVoteIDThreadID) threadIDField]

threadVoteTable ::
  TableDefinition
    (HasKey ThreadVoteID)
    ThreadVoteWrite
    ThreadVoteRead
threadVoteTable =
  addTableConstraints
    [threadVoteToUserForeignKeyConstraint, threadVoteToThreadForeignKeyConstraint]
    ( mkTableDefinition
        "vote_thread"
        userThreadCompositeKey
        threadVoteMarshaller
    )
  where
    threadVoteToUserForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName userIDField)
            (fieldName userIDField)
            :| []
        )
        (defaultForeignKeyOptions {foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})

    threadVoteToThreadForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier threadTable)
        ( foreignReference
            (fieldName threadIDField)
            (fieldName threadIDField)
            :| []
        )
        (defaultForeignKeyOptions {foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})

commentTable ::
  TableDefinition
    (HasKey CommentID)
    CommentWrite
    CommentRead
commentTable =
  addTableConstraints
    [commentToUserForeignKeyConstraint, commentToThreadForeignKeyConstraint]
    ( mkTableDefinition
        "comment"
        (primaryKey commentIDField)
        commentMarshaller
    )
  where
    commentToUserForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName userIDField)
            (fieldName userIDField)
            :| []
        )
        (defaultForeignKeyOptions {foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})

    commentToThreadForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier threadTable)
        ( foreignReference
            (fieldName threadIDField)
            (fieldName threadIDField)
            :| []
        )
        (defaultForeignKeyOptions {foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})

userCommentCompositeKey :: PrimaryKey CommentVoteID
userCommentCompositeKey =
  compositePrimaryKey
    (primaryKeyPart (\CommentVoteID {..} -> commentVoteIDUserID) userIDField)
    [primaryKeyPart (\CommentVoteID {..} -> commentVoteIDCommentID) commentIDField]

commentVoteTable ::
  TableDefinition
    (HasKey CommentVoteID)
    CommentVoteWrite
    CommentVoteRead
commentVoteTable =
  addTableConstraints
    [commentVoteToUserForeignKeyConstraint, commentVoteToCommentForeignKeyConstraint]
    ( mkTableDefinition
        "vote_comment"
        userCommentCompositeKey
        commentVoteMarshaller
    )
  where
    commentVoteToUserForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName userIDField)
            (fieldName userIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade,
              foreignKeyOptionsOnDelete = Cascade
            }
        )

    commentVoteToCommentForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier commentTable)
        ( foreignReference
            (fieldName commentIDField)
            (fieldName commentIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade,
              foreignKeyOptionsOnDelete = Cascade
            }
        )

userEmailVerifyOTPTable ::
  TableDefinition
    (HasKey UserID)
    UserEmailVerifyOTPWrite
    UserEmailVerifyOTPRead
userEmailVerifyOTPTable =
  addTableConstraints
    [uevoToUserForeignKeyConstraint]
    ( mkTableDefinition
        "user_email_verify_otp"
        (primaryKey userIDField)
        userEmailVerifyOTPMarshaller
    )
  where
    uevoToUserForeignKeyConstraint =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName userIDField)
            (fieldName userIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade,
              foreignKeyOptionsOnDelete = Cascade
            }
        )
