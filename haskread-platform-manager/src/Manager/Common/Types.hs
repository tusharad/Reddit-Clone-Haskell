{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Manager.Common.Types (
    BuckectName
  , ObjectName
  , ServiceName
  , ManagerConfig (..)
 ) where

import Dhall

type BuckectName = Text
type ObjectName = Text
type ServiceName = String

data ManagerConfig = ManagerConfig {
    bucketName :: Text
  , uiObjectPath :: Text
  , beObjectPath :: Text
  , uiLocation :: FilePath
  , beLocation :: FilePath
  , uiServiceName :: String
  , beServiceName :: String
  , dbDockerName :: String
  , downloadsPath :: FilePath
  , bearerToken :: String
 } deriving (Generic, FromDhall, Show)
