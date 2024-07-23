module Platform.Handler (
    checkHealthH
) where

import Platform.Common.AppM
import Control.Monad.IO.Class

checkHealthH :: MonadIO m => AppM m String
checkHealthH = return "OK"
