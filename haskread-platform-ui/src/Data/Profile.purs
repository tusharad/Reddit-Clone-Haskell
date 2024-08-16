module Data.Profile
  ( Avatar
  , Profile
  , UserID
  , UserName
  , ProfileRep
  )
  where

import Prelude

newtype UserID = UserID Int
newtype UserName = UserName String
newtype Avatar = Avatar String
newtype Email = Email String

derive newtype instance eqUserName :: Eq UserName
derive newtype instance eqUserID :: Eq UserID
derive newtype instance eqAvatar :: Eq Avatar

type ProfileRep row = (
    userID :: UserID
   , userName :: UserName
  , image :: Avatar
  | row
 )

type Profile = { | ProfileRep ()}
type ProfileWithEmail =  { | ProfileRep (email :: Email) }