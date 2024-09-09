module Common.BulmaUtils where

import Prelude
import Bulma.Common as B
import Halogen.HTML.Properties as HP
import Halogen (ClassName(..))

className
  :: forall r i. B.ClassName
  -> HP.IProp ( "class" :: String | r) i
className =
    HP.class_ <<< ClassName <<< B.runClassName

classNames
  :: forall r i. Array B.ClassName
  -> HP.IProp ( "class" :: String | r) i
classNames =
  HP.class_ <<< ClassName <<< B.runClassNames