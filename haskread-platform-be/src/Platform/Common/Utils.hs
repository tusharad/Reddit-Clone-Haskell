{-# LANGUAGE RecordWildCards #-}

module Platform.Common.Utils
  ( toUserInfo,
  )
where

import Platform.Auth.Types
import Platform.DB.Model

toUserInfo :: UserRead -> UserInfo
toUserInfo User {..} = UserInfo userID userName
