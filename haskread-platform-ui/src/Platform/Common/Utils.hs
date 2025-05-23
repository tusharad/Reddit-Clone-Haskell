{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
  , disabled
  , getRedirectUrl
  , hush
  , genUserContext
  , getTokenAndUser
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (IOE)
import Platform.Common.Request (getUserCommentVotes, getUserInfo, getUserThreadVotes)
import Platform.Common.Types
import System.Environment (getArgs)
import Web.Hyperbole
import Web.View.Style
import Web.View.Types (ClassName)

genUserContext :: (Hyperbole :> es, IOE :> es) => [Int] -> [Int] -> Eff es (Maybe UserContext)
genUserContext threadIds commentIds = do
  mbTokenAndUser <- getTokenAndUser
  case mbTokenAndUser of
    Nothing -> pure Nothing
    Just (token, userProfile) -> do
      userCommentsReaction <- case commentIds of
        [] -> pure []
        _ -> do
          eUserCommentVotes <- liftIO $ getUserCommentVotes token commentIds
          case eUserCommentVotes of
            Left err -> do
              liftIO $ putStrLn $ "Something went wrong while fetching user comment votes: " <> err
              pure []
            Right (FetchVoteCommentsForUserResponse res) -> pure res
      eThreadVotes <- liftIO $ getUserThreadVotes token threadIds
      votes <- case eThreadVotes of
        Left err -> do
          liftIO (putStrLn $ "something went wrong while fetching user votes: " ++ err)
          pure []
        Right votes -> pure votes
      pure
        ( Just
            ( UserContext
                { ucToken = token
                , ucUserProfile = userProfile
                , ucUserThreadVotes = votes
                , ucUserCommentVotes = userCommentsReaction
                }
            )
        )

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
    []

toText :: Show a => a -> Text
toText = T.pack . show

stringToUrl :: String -> Url
stringToUrl = url . T.pack

getThreadIds :: FetchAllThreadsResponse -> [Int]
getThreadIds (FetchAllThreadsResponse {threads = t}) = map threadIDForThreadInfo t

disabled :: Mod id
disabled = att "disabled" mempty

getRedirectUrl :: IO Url
getRedirectUrl = do
  argList <- getArgs
  case argList of
    (envType : _) ->
      if envType == "local"
        then pure "http://localhost:8085/api/v1/user/oauth2/login"
        else pure "/api/v1/user/oauth2/login"
    _ -> pure "/api/v1/user/oauth2/login"

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

getTokenAndUser ::
  (Hyperbole :> es, IOE :> es) =>
  Eff es (Maybe (Text, UserProfileResponse))
getTokenAndUser = do
  mJwtToken <- jToken <$> session @AuthData
  eUserInfo <- liftIO $ maybe (pure $ Left "token not found") getUserInfo mJwtToken
  if isLeft eUserInfo
    then do
      (deleteSession @AuthData)
      pure Nothing
    else
      pure $ mJwtToken >>= (\token -> (token,) <$> hush eUserInfo)
