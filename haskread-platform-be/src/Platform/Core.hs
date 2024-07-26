{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Platform.Core (startApp, app) where

-- Starting point of the Application

import Control.Monad.Reader
import Network.Wai.Handler.Warp
import qualified Orville.PostgreSQL as O
import Orville.PostgreSQL.Raw.Connection
import Platform.API
import Platform.Common.AppM
import Platform.Common.Types
import Servant
import Servant.Auth.Server

runAppM :: MyAppState -> AppM IO a -> Handler a
runAppM myAppState appM = Handler $ runMyExceptT $ runReaderT (getApp appM) myAppState

allServer :: CookieSettings -> JWTSettings -> MyAppState -> Server (MainAPI auths)
allServer cookieSett jwtSett myAppState =
  hoistServerWithContext
    (Proxy :: Proxy (MainAPI '[JWT, Cookie]))
    (Proxy :: Proxy '[CookieSettings, JWTSettings])
    (runAppM myAppState)
    ( mainServer
        cookieSett
        jwtSett
    )

connectionOptions :: ConnectionOptions
connectionOptions =
  ConnectionOptions
    { connectionString =
        "dbname=haskread_dev_db host=localhost user=tushar password=1234",
      connectionNoticeReporting = DisableNoticeReporting,
      connectionPoolStripes = OneStripePerCapability,
      connectionPoolMaxConnections = MaxConnectionsPerStripe 1,
      connectionPoolLingerTime = 10
    }

startApp :: IO ()
startApp = do
  putStrLn "Application running at port 8085"
  pool <- createConnectionPool connectionOptions
  jwtSecretKey <- generateKey
  let orvilleState = O.newOrvilleState O.defaultErrorDetailLevel pool
      appST = MyAppState (AppConfig "uploads") orvilleState
      jwtSett = defaultJWTSettings jwtSecretKey
  let ctx = defaultCookieSettings :. jwtSett :. EmptyContext
  run 8085 (app appST jwtSett ctx)

app :: MyAppState -> JWTSettings -> Context [CookieSettings, JWTSettings] -> Application
app appST jwtSett ctx =
  serveWithContext
    (Proxy :: Proxy (MainAPI '[JWT, Cookie]))
    ctx
    (allServer defaultCookieSettings jwtSett appST)
