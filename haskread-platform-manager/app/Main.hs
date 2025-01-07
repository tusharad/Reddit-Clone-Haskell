module Main (main) where

import Manager.Server (startApp)
import System.Environment
import qualified Data.Text as T
import Control.Exception 
import Dhall
import Manager.Common.Types 

main :: IO ()
main = do 
  args <- getArgs
  if null args then do 
    putStrLn "Please provide arguments"
  else do 
    eEnv <-
      try $ input auto (T.pack $ head args) ::
        IO (Either SomeException ManagerConfig)
    case eEnv of
      Left e -> do 
        putStrLn $ "Error while reading env: " <> show e
      Right managerConfig -> do
        putStrLn $ "Environment loaded successfully: " <> head args
        startApp managerConfig
