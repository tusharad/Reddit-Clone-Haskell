module Platform.Core (startApp) where
-- Starting point of the Application
import Servant
import Network.Wai.Handler.Warp
import Platform.API

app :: Application
app = serve (Proxy :: Proxy MainAPI) mainServer

startApp :: IO ()
startApp = do
    putStrLn "Application running at port 8085"
    run 8085 app