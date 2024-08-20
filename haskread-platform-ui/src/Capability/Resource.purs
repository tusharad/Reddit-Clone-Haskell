module Capability.Resource where

import Halogen (HalogenM, lift)
import Common.Types (PaginatedArray,Thread)
import Data.Maybe (Maybe)

import Prelude

class Monad m <= ManageThreads m where
  getThreads :: m (Maybe (PaginatedArray Thread))

instance manageThreadHalogenM :: ManageThreads m => ManageThreads (HalogenM st act slots msg m) where
  getThreads = lift getThreads

