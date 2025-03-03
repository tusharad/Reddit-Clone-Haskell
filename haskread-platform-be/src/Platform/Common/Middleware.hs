{-# LANGUAGE OverloadedStrings #-}

module Platform.Common.Middleware
  ( initRateLimitTable
  , sqliteBackend
  , myCorsMiddleware
  , rateLimitMiddleware
  , concatMiddleware
  ) where

import Database.SQLite.Simple
import Network.Wai.RateLimit.Backend
import Network.Wai.Middleware.Cors
import Network.Wai.RateLimit.Strategy
import Network.Wai.RateLimit (rateLimiting)
import Network.Wai

concatMiddleware :: [Middleware] -> Middleware
concatMiddleware = foldr (.) id

myCorsPolicy :: CorsResourcePolicy
myCorsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    }

-- Apply the CORS middleware to your application
myCorsMiddleware :: Middleware
myCorsMiddleware = cors (const $ Just myCorsPolicy)

type Seconds = Integer
type Reqs = Integer

rateLimitMiddleware :: Seconds -> Reqs -> Middleware
rateLimitMiddleware secs reqs = do
  let backend = sqliteBackend "rate_limit.db"
  let strategy = fixedWindow backend (fromIntegral secs) reqs (\req -> pure $ show (remoteHost req))
  rateLimiting strategy

initRateLimitTable :: String -> IO ()
initRateLimitTable dbName =
  withConnection dbName $ \conn ->
    withImmediateTransaction conn $
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS rate_limit (key TEXT PRIMARY KEY, usage INTEGER NOT NULL DEFAULT 0,expires_at TIMESTAMP)"

sqliteBackend :: String -> Backend String
sqliteBackend dbName =
  MkBackend
    { backendGetUsage = getCount
    , backendIncAndGetUsage = incAndGetCount
    , backendExpireIn = setExpirationSeconds
    }
  where
    getCount ipAddr = do
      rows <- withConnection dbName $ \conn ->
        withImmediateTransaction conn $
        query
          conn
          "SELECT usage FROM rate_limit WHERE key = ? AND (expires_at IS NULL OR expires_at > DATETIME('now'))"
          (Only ipAddr)
      case rows of
        [Only usage] -> pure usage
        _ -> pure 0
    incAndGetCount ipAddr increment = do
      rows <- withConnection dbName $ \conn ->
        withImmediateTransaction conn $
        query
          conn
          "SELECT usage FROM rate_limit WHERE key = ? AND (expires_at IS NULL OR expires_at > DATETIME('now'))"
          (Only ipAddr)
      case rows of
        [Only usage] -> do
          let newUsage = usage + increment
          withConnection dbName $ \conn ->
           withImmediateTransaction conn $
            execute
              conn
              "UPDATE rate_limit SET usage = ? WHERE key = ?"
              (newUsage, ipAddr)
          pure newUsage
        _ -> do
          withConnection dbName $ \conn ->
           withImmediateTransaction conn $
            execute
              conn
              "DELETE FROM rate_limit WHERE key = ? AND expires_at <= DATETIME('now')"
              (Only ipAddr)
          withConnection dbName $ \conn ->
           withImmediateTransaction conn $
            execute
              conn
              "INSERT INTO rate_limit (key, usage, expires_at) VALUES (?, ?, NULL)"
              (ipAddr, increment)
          pure increment
    setExpirationSeconds seconds ipAddr =
      withConnection dbName $ \conn ->
           withImmediateTransaction conn $
        execute
          conn
          "UPDATE rate_limit SET expires_at = DATETIME('now', '+' || ? || ' seconds') WHERE key = ?"
          (seconds, ipAddr)
