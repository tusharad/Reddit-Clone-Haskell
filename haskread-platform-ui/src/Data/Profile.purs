module Data.Profile (Profile,ProfileRep,UserName,Avatar,UserID) where


newtype UserID = UserID Int
newtype UserName = UserName String
newtype Avatar = Avatar String
newtype Email = Email String

type ProfileRep row = (
    userID :: UserID
   , userName :: UserName
  , image :: Avatar
  | row
 )

type Profile = { | ProfileRep ()}
type ProfileWithEmail =  { | ProfileRep (email :: Email) }