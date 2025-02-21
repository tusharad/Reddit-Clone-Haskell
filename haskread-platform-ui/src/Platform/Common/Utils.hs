{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  , getRedirectUrl 
  , hush
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Platform.Common.Types
import Web.Hyperbole
import Web.View.Style
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
    [     ]

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

getRedirectUrl :: IO Url
getRedirectUrl = do
  argList <- getArgs
  if null argList || (head argList == "local") then 
    return "http://localhost:8085/api/v1/user/oauth2/login"
  else return "/api/v1/user/oauth2/login"

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
