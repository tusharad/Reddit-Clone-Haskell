{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Text (Text)
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
  data Action CommunityId = Init Text
    deriving (Show, Read, ViewAction)

  update (Init _) = pure communityListView

newtype SortMenuId = SortMenuId Int
  deriving (Show, Read, ViewId)

instance HyperView SortMenuId es where
  data Action SortMenuId = ChangeSort
    deriving (Show, Read, ViewAction)

  update ChangeSort = do
    pure sortMenuView

footerView :: View FooterId ()
footerView = do
  el (cc "footer-bg mt-auto py-4") $ do
    el (cc "container mx-auto text-center") $ do
      tag "p" (cc "text-white") $ do
        tag "strong" mempty $ do
          "HaskRead"
        link
          "https://www.linkedin.com/in/tushar-adhatrao/"
          (cc "text-blue-300 hover:underline")
          "Tushar Adhatrao"
        text "The source code is licensed under"
        link "https://opensource.org/license/mit" (cc "text-blue-300 hover:underline") "MIT"

showCommunityNames :: Int -> [Text] -> View CommunityId ()
showCommunityNames _ [] = none
showCommunityNames n (x : xs) = do
  tag "li" (cc "mb-2") $ do
    button (Init "xyz") (cc "block text-blue-600 hover:underline") (text x)
    tag "hr" (cc "border-gray-300") none
    showCommunityNames (n + 1) xs

communityListView :: View CommunityId ()
communityListView = do
  el (addClass (cls "card-bg shadow-lg rounded-lg mb-6 overflow-hidden")) $ do
    el (addClass (cls "border-b p-4")) $ do
      el (addClass (cls "text-lg font-bold text-gray-800")) $ do
        text "Communities"
        el (cc "p-4") $ do
          tag "ul" (cc "space-y-2") $ do
            showCommunityNames 1 ["Haskell", "Functional programming"]

sortMenuView :: View SortMenuId ()
sortMenuView = do
  el (cc "flex justify-center mb-6") $ do
    tag "ul" (cc "flex space-x-4 bg-white rounded-full shadow-lg") $ do
      tag "li" (cc "relative" <> att "id" "topVotedButton") $ do
        button
          ChangeSort
          (cc topVotedBtnCSS)
          $ do
            text "Top voted"
            el
              ( att "id" "topVotedButton"
                  <> cc "absolute z-10 mt-2 bg-white rounded-md shadow-lg hidden"
              )
              $ do
                tag "ul" (cc "py-1") $ do
                  tag "li" mempty $ do
                    tag "p" (cc "block px-4 py-2 text-gray-800 hover:bg-blue-100") "Top of day"
                  tag "li" mempty $ do
                    tag "p" (cc "block px-4 py-2 text-gray-800 hover:bg-blue-100") "Top of month"
                  tag "li" mempty $ do
                    tag "p" (cc "block px-4 py-2 text-gray-800 hover:bg-blue-100") "Top of all time"
      tag "li" (cc reallyLongCSS) "Trending"
      tag "li" (cc reallyLongCSS) "New"
      tag "li" (cc reallyLongCSS) "Following"
