{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.View
  ( FooterId (..)
  , CommunityId (..)
  , SortMenuId (..)
  , footerView
  , communityListView
  , sortMenuView
  ) where

import Platform.Common.Types
import Platform.Common.Utils
import Web.Hyperbole
import Web.View.Style

newtype FooterId = FooterId Int
  deriving (Show, Read, ViewId)

instance HyperView FooterId es where
  data Action FooterId
    = InitFooter
    deriving (Show, Read, ViewAction)
  update InitFooter = pure footerView

newtype CommunityId = CommunityId Int
  deriving (Show, Read, ViewId)

instance HyperView CommunityId es where
  data Action CommunityId = Init
    deriving (Show, Read, ViewAction)

newtype SortMenuId = SortMenuId Int
  deriving (Show, Read, ViewId)

instance HyperView SortMenuId es where
  data Action SortMenuId = ChangeSort
    deriving (Show, Read, ViewAction)

  update ChangeSort = do
    pure sortMenuView

footerView :: View FooterId ()
footerView = do
  tag "footer" (cc "bg-blue-800 mt-auto py-4") $ do
    el (cc "container mx-auto text-center") $ do
      tag "p" (cc "text-white") $ do
        tag "strong" mempty "HaskRead"
        link
          "https://www.linkedin.com/in/tushar-adhatrao/"
          (cc "text-blue-300 hover:text-blue-400 transition")
          "Tushar Adhatrao"
        text "The source code is licensed under "
        link "https://opensource.org/license/mit" (cc "text-blue-300 hover:text-blue-400 transition") "MIT"

showCommunityNames :: [CommunityC] -> View CommunityId ()
showCommunityNames [] = none
showCommunityNames (CommunityC {..} : communityList) = do
  tag "li" (cc "border-b last:border-b-0 dark:border-gray-700") $ do
    link
      (url $ "/?communityId=" <> toText communityID)
      ( cc
          "block p-2 text-blue-600 dark:text-blue-400 hover:bg-gray-100 dark:hover:bg-gray-700 hover:underline transition"
      )
      (text communityName)
    showCommunityNames communityList

communityListView :: Communities -> View CommunityId ()
communityListView (Communities communityList) = do
  el (cc "w-full px-4") $ do
    el (addClass (cls "bg-white dark:bg-gray-800 shadow-lg rounded-lg mb-6 overflow-hidden")) $ do
      el (addClass (cls "border-b p-4 dark:border-gray-700")) $ do
        el (addClass (cls "text-lg font-bold text-gray-800 dark:text-gray-200")) (text "Communities")
        tag "ul" (cc "p-4 space-y-2") $ do
          showCommunityNames communityList

sortMenuView :: View SortMenuId ()
sortMenuView = do
  el (cc "flex justify-center mb-6") $ do
    tag "ul" (cc "flex space-x-4 bg-white dark:bg-gray-800 rounded-full shadow-lg p-2") $ do
      tag "li" (cc "relative" . att "id" "topVotedButton") $ do
        button
          ChangeSort
          (cc topVotedBtnCSS)
          $ do
            text "Top voted"
            el
              ( att "id" "topVotedButton"
                  <> cc "absolute z-10 mt-2 bg-white dark:bg-gray-800 rounded-md shadow-lg hidden"
              )
              $ do
                tag "ul" (cc "py-1") $ do
                  tag "li" mempty $ do
                    tag
                      "p"
                      ( cc
                          "block w-full text-left px-4 py-2 text-gray-800 dark:text-gray-200 hover:bg-blue-100 dark:hover:bg-blue-700 transition"
                      )
                      "Top of day"
                  tag "li" mempty $ do
                    tag
                      "p"
                      ( cc
                          "block w-full text-left px-4 py-2 text-gray-800 dark:text-gray-200 hover:bg-blue-100 dark:hover:bg-blue-700 transition"
                      )
                      "Top of month"
                  tag "li" mempty $ do
                    tag
                      "p"
                      ( cc
                          "block w-full text-left px-4 py-2 text-gray-800 dark:text-gray-200 hover:bg-blue-100 dark:hover:bg-blue-700 transition"
                      )
                      "Top of all time"
      tag "li" (cc reallyLongCSS) "Trending"
      tag "li" (cc reallyLongCSS) "New"
      tag "li" (cc reallyLongCSS) "Following"
