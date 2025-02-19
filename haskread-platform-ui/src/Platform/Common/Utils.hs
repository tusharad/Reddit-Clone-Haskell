{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Platform.Common.Utils
  ( btn
  , btn'
  , invalid
  , success
  , inputS
  , globalCSS
  , cc
  , toText
  , stringToUrl
  , getThreadIds
  , topVotedBtnCSS
  , reallyLongCSS
  , disabled 
  , reqParamMaybe
  , getRedirectUrl 
  , hush
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Platform.Common.Types
import Web.Hyperbole
import Web.View.Style
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Web.Internal.HttpApiData
import System.Environment (getArgs)

btn :: Mod id
btn = btn' Primary

btn' :: AppColor -> Mod id
btn' clr = bg clr . hover (bg (hovClr clr)) . color (txtClr clr) . pad 10
  where
    hovClr Primary = PrimaryLight
    hovClr c = c
    txtClr _ = White

invalid :: Mod id
invalid = color Danger

success :: Mod id
success = color Success

inputS :: Mod id
inputS = cc "w-full px-3 py-2 border rounded"

cc :: ClassName -> Mod id
cc txt = addClass $ cls txt

globalCSS :: Text
globalCSS =
  T.unlines
    [ "body { background-color: #F4EEFF;}"
    , ".navbar-bg { background: linear-gradient(90deg, #424874, #DCD6F7); }"
    , ".card-bg {background: linear-gradient(120deg, #DCD6F7, #F4EEFF);}"
    , ".footer-bg { background-color: #424874; }"
    ]

toText :: Show a => a -> Text
toText = T.pack . show

stringToUrl :: String -> Url
stringToUrl = url . T.pack

getThreadIds :: FetchAllThreadsResponse -> [Int]
getThreadIds (FetchAllThreadsResponse {threads = t}) = map threadIDForThreadInfo t

topVotedBtnCSS :: ClassName
topVotedBtnCSS =
  "px-4 py-2 rounded-full bg-blue-600 text-white font-semibold cursor-pointer shadow"

reallyLongCSS :: ClassName
reallyLongCSS = "px-4 py-2 rounded-full text-gray-800 hover:bg-blue-100 cursor-pointer transition shadow"

disabled :: Mod id
disabled = att "disabled" mempty

reqParamMaybe :: forall a es. (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es (Maybe a)
reqParamMaybe p = do
  q <- reqParams
  pure $ lookup (encodeUtf8 p) q >>= \case
    Nothing -> Nothing
    Just v -> either (const Nothing) Just $ parseQueryParam (decodeUtf8 v)

getRedirectUrl :: IO Url
getRedirectUrl = do
  argList <- getArgs
  if null argList || (head argList == "local") then 
    return "http://localhost:8085/api/v1/user/oauth2/login"
  else return "/api/v1/user/oauth2/login"

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
