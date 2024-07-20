module Platform.Handler (
    checkHealthH
) where

import Servant

checkHealthH :: Handler String
checkHealthH = return "OK"