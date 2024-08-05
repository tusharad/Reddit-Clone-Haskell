{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Platform.Admin.Community.Handler
  ( communityCreateH,
    communityUpdateH,
    communityDeleteH,
  )
where

import Control.Monad (void, when)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (nub)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Platform.Admin.Community.DB
import Platform.Admin.Community.Types
import Platform.Auth.Types
import Platform.Common.AppM
import Platform.Common.Utils
import Platform.DB.Model
import Servant.Auth.Server (AuthResult (..))
import UnliftIO

communityCreateH ::
  (MonadUnliftIO m) =>
  AuthResult AdminInfo ->
  CommunityCreateReqBody ->
  AppM m CommunityCreateResponse
communityCreateH (Authenticated _) c@CommunityCreateReqBody {..} = do
  void $ checkIfCommunityNameExists communityNameForCreate
  void $ checkDescriptionNotEmpty communityDescriptionForCreate
  void $ validateLabelList communityLabelListForCreate
  addCommunity c
communityCreateH _ _ = throw401Err "Please login first"

addCommunity :: (MonadUnliftIO m) => CommunityCreateReqBody -> AppM m CommunityCreateResponse
addCommunity CommunityCreateReqBody {..} = do
  let communityWrite =
        Community
          { communityName = communityNameForCreate,
            communityDescription = communityDescriptionForCreate,
            communityLabelList = toJsonbArray communityLabelListForCreate,
            communityCreatedAt = (),
            communityUpdatedAt = (),
            communityID = ()
          }
  (eRes :: Either SomeException ()) <- try $ addCommunityQ communityWrite
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ ->
      return
        CommunityCreateResponse
          { communityCreateResponseMsg = "Community added successfully!"
          }

checkIfCommunityNameExists :: (MonadUnliftIO m) => T.Text -> AppM m ()
checkIfCommunityNameExists cName = do
  eRes :: Either SomeException (Maybe CommunityRead) <- 
            try $ fetchCommunityByNameQ cName
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right mCommunity -> when (isJust mCommunity) $ 
        throw400Err "Community name already exists!"

checkDescriptionNotEmpty :: (MonadUnliftIO m) => T.Text -> AppM m ()
checkDescriptionNotEmpty cDescription = do
  when (T.null cDescription) $
    throw400Err "Community description cannot be empty!"

validateLabelList :: (MonadUnliftIO m) => [T.Text] -> AppM m ()
validateLabelList communityLabelListForCreate = do
  when (length communityLabelListForCreate > 10) $
    throw400Err "Community label list cannot have more than 10 tags!"
  when (any (\x -> T.length x > 50) communityLabelListForCreate) $
    throw400Err
      "Each tag in community label list cannot be more than 50 characters long!"
  when
    ( length communityLabelListForCreate
        /= length (nub communityLabelListForCreate)
    )
    $ throw400Err "Each tag in community label list should be unique!"

fetchCommunityByID :: (MonadUnliftIO m) => CommunityID -> AppM m (Maybe CommunityRead)
fetchCommunityByID communityID = do
  eRes :: Either SomeException (Maybe CommunityRead) <-
    try $ fetchCommunityByIDQ communityID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right mCommunity -> pure mCommunity

communityUpdateH ::
  (MonadUnliftIO m) =>
  AuthResult AdminInfo ->
  CommunityUpdateReqBody ->
  AppM m CommunityUpdateResponse
communityUpdateH (Authenticated _) CommunityUpdateReqBody {..} = do
  mCommunity <- fetchCommunityByID communityIDForUpdate
  case mCommunity of
    Nothing -> throw400Err "Community not found!"
    Just _ -> do
      void $ checkDescriptionNotEmpty communityDescriptionForUpdate
      void $ validateLabelList communityLabelListForUpdate
      updateCommunity
  where
    updateCommunity = do
      let updatedCommunity =
            Community
              { communityName = communityNameForUpdate,
                communityDescription = communityDescriptionForUpdate,
                communityLabelList = toJsonbArray communityLabelListForUpdate,
                communityCreatedAt = (),
                communityUpdatedAt = (),
                communityID = ()
              }
      eRes :: Either SomeException () <-
        try $
          updateCommunityQ communityIDForUpdate updatedCommunity
      case eRes of
        Left e -> throw400Err $ BSL.pack $ show e
        Right _ ->
          return
            CommunityUpdateResponse
              { communityUpdateResponseMsg = "Community updated successfully!"
              }
communityUpdateH _ _ = throw401Err "Please login first"

data MyCustomException = MyCustomException T.Text
  deriving (Show)

instance Exception MyCustomException

deleteCommunity :: (MonadUnliftIO m) => CommunityID -> AppM m CommunityDeleteResponse
deleteCommunity communityID = do
  eRes :: Either SomeException () <- try $ deleteCommunityQ communityID
  case eRes of
    Left e -> throw400Err $ BSL.pack $ show e
    Right _ -> do
      return
        CommunityDeleteResponse
          { communityDeleteResponseMsg = "Community deleted successfully!"
          }

communityDeleteH ::
  (MonadUnliftIO m) =>
  AuthResult AdminInfo ->
  CommunityID ->
  AppM m CommunityDeleteResponse
communityDeleteH (Authenticated _) communityID = do
  mCommunity <- fetchCommunityByID communityID
  case mCommunity of
    Nothing -> throw400Err "Community not found!"
    Just _ -> do
      deleteCommunity communityID
communityDeleteH _ _ = throw401Err "Please login first"
