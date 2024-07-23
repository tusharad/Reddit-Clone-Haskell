module Platform.Core (startApp) where
-- Starting point of the Application
import Servant
import Network.Wai.Handler.Warp
import Platform.API
import Platform.Common.AppM
import Platform.Common.Types
import Control.Monad.Reader
import Orville.PostgreSQL.Raw.Connection
import qualified Orville.PostgreSQL as O



runAppM :: MyAppState -> AppM IO a -> Handler a
runAppM myAppState appM = Handler $ runReaderT (getApp appM) myAppState 

allServer :: MyAppState -> Server MainAPI
allServer myAppState = hoistServer (Proxy :: Proxy MainAPI) (runAppM myAppState) mainServer

app :: MyAppState -> Application
app myAppState = serve (Proxy :: Proxy MainAPI) (allServer myAppState)

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
    let orvilleState = O.newOrvilleState O.defaultErrorDetailLevel pool
    let appST = MyAppState (AppConfig "uploads") orvilleState
    run 8085 (app appST) 
