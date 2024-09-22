module Capability.Resource where

import Prelude

import Common.Types (ChangePasswordFields, Community, CreateThreadFields, DeleteUserFields, LoginFields, MyRoute, NestedComment, OtpFields, PaginatedArray, Profile, RegisterFields, Thread, UpdateThreadFields, Pagination)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageThreads m where
  getThreads :: Pagination -> m (Maybe (PaginatedArray Thread))
  createThread :: CreateThreadFields -> m (Maybe String)
  deleteThread :: Int -> m (Maybe String)
  getThread :: Int -> m (Maybe Thread)
  updateThread :: UpdateThreadFields -> m (Maybe String)

instance manageThreadHalogenM :: ManageThreads m => ManageThreads (HalogenM st act slots msg m) where
  getThreads = lift <<< getThreads
  createThread = lift <<< createThread
  deleteThread = lift <<< deleteThread
  getThread = lift <<< getThread
  updateThread = lift <<< updateThread

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

class Monad m <= Navigate m where
  navigate :: MyRoute -> m Unit

instance navigateHalogenM ::
  Navigate m =>
  Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate

class Monad m <= ManageComments m where
    getCommentsByThreadID :: Int -> m (Maybe (PaginatedArray NestedComment))

instance manageCommentsHalogenM :: ManageComments m 
    => ManageComments (HalogenM st act slots msg m) where
  getCommentsByThreadID = lift <<< getCommentsByThreadID

class Monad m <= ManageCommunity m where
  getCommunities :: m (Maybe (PaginatedArray Community))

instance manageCommunityHalogenM :: ManageCommunity m => ManageCommunity (HalogenM st act slots msg m) where
  getCommunities = lift getCommunities