{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Platform.View.LiveSearch
  ( LiveSearchId (..)
  , liveSearch
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text (Text, append)
import qualified Data.Text as T
import Effectful (IOE)
import Platform.Common.Request
import Platform.Common.Types
import Platform.Common.Utils
import Web.Hyperbole hiding (input, textarea)

newtype LiveSearchId = LiveSearchId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView LiveSearchId es where
  data Action LiveSearchId
    = SearchTerm Text
    deriving (Show, Read, ViewAction)

  update (SearchTerm searchTerm) = do
    mbThreadList <- liftIO $ getAllThreadsBySearch searchTerm
    case mbThreadList of
      Nothing -> pure $ liveSearch searchTerm []
      Just threadList -> do
        pure $ liveSearch searchTerm (threads threadList)

liveSearch :: Text -> [ThreadInfo] -> View LiveSearchId ()
liveSearch currTerm searchResult =
  stack (cc "relative") $ do
    layer $ el_ $ do
      search 
        SearchTerm 600 
        (cc "pl-2 pr-4 py-2 border rounded-full focus:ring-2 focus:ring-blue-600")
    searchPopup searchResult shownIfTerm
  where
    shownIfTerm = if T.null currTerm then hide else flexCol

searchPopup :: [ThreadInfo] -> Mod LiveSearchId -> Layer LiveSearchId ()
searchPopup [] f = do
  popout (offset (TRBL 50 0 0 0) . border 1 . bg White . f) $ do
    el (hover (bg Primary) . pad 5 . bg White) $ text "No results found"
searchPopup threadInfoList f = do
  popout (offset (TRBL 50 0 0 0) . border 1 . bg White . f) $ do
    forM_ threadInfoList $ \thread -> do
      link
        (url $ "/view-thread/" `append` toText (threadIDForThreadInfo thread))
        (hover (bg Primary) . pad 5 . bg White)
        $ do
          text (title thread)
          el (cc "text-gray-500") $ text $ T.take 20 (fromMaybe "" (description thread))
