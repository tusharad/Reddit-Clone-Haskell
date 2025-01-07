{- Module containing functions for interacting with Systemd apis -}
module Manager.Systemd (
    restartService
  , stopService
 ) where

import System.Process
import Control.Exception (try)
import Manager.Common.Types

asSudo :: String -> String
asSudo = (<>) "sudo " 

restartService :: ServiceName -> IO (Either String String)
restartService serviceName = do 
  eRes <- try $ callCommand (asSudo "systemctl restart " <> serviceName)
  case eRes of
    Left e -> pure . Left $ show (e :: IOError)
    Right _ -> pure $ Right "all good"

stopService :: ServiceName -> IO (Either String String)
stopService serviceName = do 
  eRes <- try $ callCommand (asSudo "systemctl stop " <> serviceName)
  case eRes of
    Left e -> pure . Left $ show (e :: IOError)
    Right _ -> pure $ Right "all good"
