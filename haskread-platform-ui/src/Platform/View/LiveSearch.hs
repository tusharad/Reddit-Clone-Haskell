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
{-# LANGUAGE DeriveGeneric #-}

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
import Web.View.Types (ClassName (..))
import qualified Platform.Common.CSS as CSS

newtype LiveSearchId = LiveSearchId Int
  deriving (Show, Read, ViewId, Generic)

instance IOE :> es => HyperView LiveSearchId es where
  data Action LiveSearchId
    = SearchTerm Text
    deriving (Show, Read, ViewAction, Generic)

  update (SearchTerm searchTerm) = do
    eThreadList <- liftIO $ getAllThreadsBySearch searchTerm
    case eThreadList of
      Left err -> do
        liftIO $ putStrLn $ "Error while fetching threads: " <> err
        pure $ liveSearch searchTerm []
      Right threadList -> do
        pure $ liveSearch searchTerm (threads threadList)

liveSearch :: Text -> [ThreadInfo] -> View LiveSearchId ()
liveSearch currTerm searchResult =
  stack (cc "relative w-full sm:w-64") $ do
    layer id $ el_ $ do
      search SearchTerm 600 $
        cc CSS.searchInputCSS . placeholder "Search..."
    searchPopup searchResult shownIfTerm
  where
    shownIfTerm = if T.null currTerm then "hidden" else mempty

searchPopup :: [ThreadInfo] -> ClassName -> Layer LiveSearchId ()
searchPopup [] f =
  layer (popup (TRBL 51 0 0 0) . cc (CSS.searchNoResultsCSS f)) $
    el (cc (CSS.searchPopupItemCSS <> " " <> f)) $
      text "No results found"

searchPopup threadInfoList f =
  layer (popup (TRBL 50 0 0 0) . cc (CSS.searchPopupContainerCSS f)) $ do
    forM_ threadInfoList $ \thread -> do
      link
        (url $ "/view-thread/" `append` toText (threadIDForThreadInfo thread))
        (hover (bg Primary) . pad 5 . bg White)
        $ do
          text (title thread)
          el (cc CSS.searchResultDescriptionCSS) $
            text $ T.take 20 (fromMaybe "" (description thread))
