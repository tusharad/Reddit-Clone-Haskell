module Capability.Resource where

import Halogen (HalogenM, lift)
import Common.Types (
      PaginatedArray
    , Thread
    , LoginFields
    , Profile
    , RegisterFields
    , OtpFields
    , CreateThreadFields
    , ChangePasswordFields
    , DeleteUserFields 
    )
import Data.Maybe (Maybe)
import Data.Either (Either)

import Prelude

class Monad m <= ManageThreads m where
  getThreads :: m (Maybe (PaginatedArray Thread))
  createThread :: CreateThreadFields -> m (Maybe String)
  deleteThread :: Int -> m (Maybe String)
  getThread :: Int -> m (Maybe Thread)

instance manageThreadHalogenM :: ManageThreads m => ManageThreads (HalogenM st act slots msg m) where
  getThreads = lift getThreads
  createThread = lift <<< createThread
  deleteThread = lift <<< deleteThread
  getThread = lift <<< getThread

class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Either String Int)
  verifyOtp :: OtpFields -> m (Either String Unit)
  changePassword :: ChangePasswordFields -> m (Maybe String)
  deleteUser :: DeleteUserFields -> m (Maybe String)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  verifyOtp = lift <<< verifyOtp
  changePassword = lift <<< changePassword
  deleteUser = lift <<< deleteUser
