module Capability.Navigate where

import Control.Monad.Trans.Class (lift)
import Prelude
import Halogen (HalogenM)
import Common.Types (MyRoute)

class Monad m <= Navigate m where
  navigate :: MyRoute -> m Unit

instance navigateHalogenM :: Navigate m => 
    Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
