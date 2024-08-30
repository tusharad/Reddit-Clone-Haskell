module Capability.Resource where

import Halogen (HalogenM, lift)
import Common.Types (PaginatedArray,Thread,LoginFields,Profile,RegisterFields,OtpFields)
import Data.Maybe (Maybe)
import Data.Either (Either)

import Prelude

class Monad m <= ManageThreads m where
  getThreads :: m (Maybe (PaginatedArray Thread))

instance manageThreadHalogenM :: ManageThreads m => ManageThreads (HalogenM st act slots msg m) where
  getThreads = lift getThreads

class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Either String Int)
  verifyOtp :: OtpFields -> m (Either String Unit)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  verifyOtp = lift <<< verifyOtp
