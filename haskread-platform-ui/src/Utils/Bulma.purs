module Utils.Bulma where

import Prelude
import Bulma.Common as B
import Halogen.HTML.Properties as HP
import Halogen (ClassName(..))

class_
  :: forall r i. B.ClassName
  -> HP.IProp ( "class" :: String | r) i
class_ =
    HP.class_ <<< ClassName <<< B.runClassName

classes_
  :: forall r i. Array B.ClassName
  -> HP.IProp ( "class" :: String | r) i
classes_ =
  HP.class_ <<< ClassName <<< B.runClassNames