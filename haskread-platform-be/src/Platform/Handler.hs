{-# LANGUAGE OverloadedStrings #-}
module Platform.Handler (
    checkHealthH
) where

import Platform.Common.AppM
import Control.Monad.IO.Class
import Platform.Log

checkHealthH :: MonadIO m => AppM m String
checkHealthH = do
    logDebug "Checking health"
    return "OK"
