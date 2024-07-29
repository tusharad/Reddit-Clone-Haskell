module Platform.Admin.Community.DB (
    fetchCommunityByNameQ
  , addCommunityQ
  , fetchCommunityByIDQ
  , updateCommunityQ
  , deleteCommunityQ
 ) where

import Data.Text (Text)
import Orville.PostgreSQL
import Platform.DB.Marshaller
import Platform.DB.Model
import Platform.DB.Table


fetchCommunityByNameQ :: (MonadOrville m) => Text ->  m (Maybe CommunityRead)
fetchCommunityByNameQ cName = findFirstEntityBy communityTable whereCommunityNameIs
  where
    whereCommunityNameIs = where_ $ communityNameField .== cName

addCommunityQ :: (MonadOrville m) => CommunityWrite -> m ()
addCommunityQ = insertEntity communityTable

fetchCommunityByIDQ :: (MonadOrville m) => CommunityID -> m (Maybe CommunityRead)
fetchCommunityByIDQ = findEntity communityTable

updateCommunityQ :: (MonadOrville m) => CommunityID -> CommunityWrite -> m ()
updateCommunityQ = updateEntity communityTable

deleteCommunityQ :: (MonadOrville m) => CommunityID -> m ()
deleteCommunityQ = deleteEntity communityTable