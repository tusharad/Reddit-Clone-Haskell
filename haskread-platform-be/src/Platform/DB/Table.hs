module Platform.DB.Table
  ( userTable,
    userProfileImageTable,
    adminTable
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
    [ foreignKeyConstraint
        (tableIdentifier userTable)
        ( foreignReference
              (fieldName userIDField)
              (fieldName userIDField)
            :| []
        )
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
