{-# LANGUAGE RecordWildCards #-}
module Platform.DB.Table
  ( userTable,
    userProfileImageTable,
    adminTable,
    communityTable,
    threadTable,
    threadVoteTable,
    commentTable
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
      (defaultForeignKeyOptions { foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})
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
    )]
    (
      mkTableDefinition
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
    )]
    (
      mkTableDefinition
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
    [ threadToUserForeignKeyConstraint,threadToCommunityForeignKeyConstraint ]
    (
      mkTableDefinition
        "thread"
        (primaryKey threadIDField)
        threadMarshaller
    )
  where
    threadToUserForeignKeyConstraint = 
      foreignKeyConstraintWithOptions
      (tableIdentifier userTable) 
      (foreignReference 
        (fieldName userIDField) (fieldName userIDField) :| [])
      (defaultForeignKeyOptions { foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})
      
    threadToCommunityForeignKeyConstraint =
      foreignKeyConstraintWithOptions
      (tableIdentifier communityTable)
      (foreignReference
        (fieldName communityIDField) (fieldName communityIDField) :| [])
      (defaultForeignKeyOptions { foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})
    
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
    [ threadVoteToUserForeignKeyConstraint,threadVoteToThreadForeignKeyConstraint ]
    (
      mkTableDefinition
        "vote_thread"
        userThreadCompositeKey
        threadVoteMarshaller
    )
  where
    threadVoteToUserForeignKeyConstraint = 
      foreignKeyConstraintWithOptions
      (tableIdentifier userTable) 
      (foreignReference 
        (fieldName userIDField) (fieldName userIDField) :| [])
      (defaultForeignKeyOptions { foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})
      
    threadVoteToThreadForeignKeyConstraint =
      foreignKeyConstraintWithOptions
      (tableIdentifier threadTable)
      (foreignReference
        (fieldName threadIDField) (fieldName threadIDField) :| [])
      (defaultForeignKeyOptions { foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})

commentTable ::
  TableDefinition
  (HasKey CommentID)
  CommentWrite
  CommentRead
commentTable =
  addTableConstraints
    [ commentToUserForeignKeyConstraint,commentToThreadForeignKeyConstraint ]
    (
      mkTableDefinition
        "comment"
        (primaryKey commentIDField)
        commentMarshaller
    )
  where
    commentToUserForeignKeyConstraint = 
      foreignKeyConstraintWithOptions
      (tableIdentifier userTable) 
      (foreignReference 
        (fieldName userIDField) (fieldName userIDField) :| [])
      (defaultForeignKeyOptions { foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})
      
    commentToThreadForeignKeyConstraint =
      foreignKeyConstraintWithOptions
      (tableIdentifier threadTable)
      (foreignReference
        (fieldName threadIDField) (fieldName threadIDField) :| [])
      (defaultForeignKeyOptions { foreignKeyOptionsOnUpdate = Cascade, foreignKeyOptionsOnDelete = Cascade})