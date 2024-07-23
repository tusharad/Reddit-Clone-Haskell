module Platform.DB.Table (
    userTable
) where

import Platform.DB.Model
import Platform.DB.Marshaller
import Orville.PostgreSQL

userTable :: TableDefinition (HasKey UserID) UserWrite UserRead
userTable = mkTableDefinition
    "users"
        (primaryKey userIDField)
            userMarshaller
