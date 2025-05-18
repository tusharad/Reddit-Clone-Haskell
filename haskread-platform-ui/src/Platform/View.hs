{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.View
  ( FooterId (..)
  , CommunityId (..)
  , SortMenuId (..)
  , footerView
  , communityListView
  , sortMenuView
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Effectful (IOE)
import Platform.Common.CSS
import Platform.Common.Request (getCommunityList)
import Platform.Common.Types
import Platform.Common.Utils
import Web.Hyperbole

newtype FooterId = FooterId Int
  deriving (Show, Read, ViewId, Generic)

instance HyperView FooterId es where
  data Action FooterId
    = InitFooter
    deriving (Show, Read, ViewAction, Generic)
  update InitFooter = pure footerView

newtype CommunityId = CommunityId Int
  deriving (Show, Read, ViewId, Generic)

instance IOE :> es => HyperView CommunityId es where
  data Action CommunityId = Init
    deriving (Show, Read, ViewAction, Generic)

  update Init = do
    eCommunityList <- liftIO getCommunityList
    pure $ communityListView_ eCommunityList

newtype SortMenuId = SortMenuId Int
  deriving (Show, Read, ViewId, Generic)

instance HyperView SortMenuId es where
  data Action SortMenuId = ChangeSort
    deriving (Show, Read, ViewAction, Generic)

  update ChangeSort = do
    pure sortMenuView

footerView :: View FooterId ()
footerView = do
  tag "footer" (cc footerBgCSS) $ do
    el (cc "container mx-auto text-center") $ do
      tag "p" (cc "text-white") $ do
        tag "strong" mempty "HaskRead"
        link linkedInLink (cc footerBrandCSS) "Tushar Adhatrao"
        text "The source code is licensed under "
        link "https://opensource.org/license/mit" (cc footerBrandCSS) "MIT"

showCommunityNames :: [CommunityC] -> View CommunityId ()
showCommunityNames [] = none
showCommunityNames (CommunityC {..} : communityList) = do
  tag "li" (cc communityNameCSS) $ do
    link
      (url $ "/?communityId=" <> toText communityID)
      (cc commonLinkCSS)
      (text communityName)
    showCommunityNames communityList

communityListView_ :: Either String Communities -> View CommunityId ()
communityListView_ eCommunityList =
  el (cc "w-full px-4") $ do
    el (cc whiteBackgroundRoundedCSS) $ do
      el (cc bottomBorderCSS) $ do
        el (cc largeTextCSS) (text "Communities")
        tag "ul" (cc "p-4 space-y-2") $ do
          either
            (el_ . raw . T.pack)
            (showCommunityNames . communities)
            eCommunityList

communityListView :: View CommunityId ()
communityListView = do
  el (cc "w-full px-4") $ do
    el (cc whiteBackgroundRoundedCSS) $ do
      el (cc bottomBorderCSS) $ do
        el (cc largeTextCSS) (text "Communities")
        tag "ul" (cc "p-4 space-y-2") $ do
          el (onLoad Init 500) $ text "Loading communities"

sortMenuView :: View SortMenuId ()
sortMenuView = do
  el (cc "flex justify-center mb-6") $ do
    tag "ul" (cc sortMenuItemCSS) $ do
      tag "li" (cc "relative" . att "id" "topVotedButton") $ do
        button ChangeSort (cc topVotedBtnCSS) $ do
          text "Top voted"
          el (att "id" "topVotedButton" <> cc selectedSortMenuItemCSS) $ do
            tag "ul" (cc "py-1") $ do
              tag "li" mempty $ tag "p" (cc dropdownItemCSS) "Top of day"
              tag "li" mempty $ tag "p" (cc dropdownItemCSS) "Top of month"
              tag "li" mempty $ tag "p" (cc dropdownItemCSS) "Top of all time"
      tag "li" (cc reallyLongCSS) "Trending"
      tag "li" (cc reallyLongCSS) "New"
      tag "li" (cc reallyLongCSS) "Following"
