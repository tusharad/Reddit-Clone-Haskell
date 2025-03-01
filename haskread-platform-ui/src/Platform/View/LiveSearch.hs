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
import Web.View.Types (ClassName (..))

newtype LiveSearchId = LiveSearchId Int
  deriving (Show, Read, ViewId)

instance IOE :> es => HyperView LiveSearchId es where
  data Action LiveSearchId
    = SearchTerm Text
    deriving (Show, Read, ViewAction)

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
      search
        SearchTerm
        600
        ( ( cc
              "w-full px-4 py-2 border rounded-full focus:ring-2 focus:ring-blue-600 dark:bg-gray-700 dark:border-gray-600 dark:text-white"
          )
            . placeholder "Search..."
        )
    searchPopup searchResult shownIfTerm
  where
    shownIfTerm = if T.null currTerm then "hidden" else mempty

searchPopup :: [ThreadInfo] -> Text -> Layer LiveSearchId ()
searchPopup [] f = do
  layer
    ( popup (TRBL 51 0 0 0)
        . cc
          ( ClassName
              ( "absolute top-full left-0 right-0 mt-1 bg-white dark:bg-gray-800 border rounded-md shadow-lg "
                  <> f
              )
          )
    )
    $ do
      el (cc (ClassName ("p-2 hover:bg-blue-100 dark:hover:bg-blue-700 transition " <> f))) $
        text "No results found"
searchPopup threadInfoList f = do
  layer
    ( popup (TRBL 50 0 0 0)
        . cc
          ( ClassName
              ( "absolute top-full left-0 right-0 mt-1 bg-white dark:bg-gray-800 border rounded-md shadow-lg "
                  <> f
              )
          )
    )
    $ do
      forM_ threadInfoList $ \thread -> do
        link
          (url $ "/view-thread/" `append` toText (threadIDForThreadInfo thread))
          (hover (bg Primary) . pad 5 . bg White)
          $ do
            text (title thread)
            el (cc "text-gray-500") $ text $ T.take 20 (fromMaybe "" (description thread))
