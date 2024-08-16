module Capability.Resource.Thread where

import Prelude

import Data.Maybe (Maybe)
import Data.Thread (Thread, PaginatedArray)
import Halogen (HalogenM, lift)

class Monad m <= ManageThread m where
    getThreads :: m (Maybe (PaginatedArray Thread))

instance manageThreadHalogenM :: ManageThread m => ManageThread (HalogenM st act slots msg m) where
    getThreads = lift getThreads