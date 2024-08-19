module Common.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root,path)
import Routing.Duplex.Generic as G

data MyRoute =
      Home
    | Login

derive instance genericRoute :: Generic MyRoute _
derive instance eqRoute :: Eq MyRoute

myRoute :: RouteDuplex' MyRoute
myRoute = root $ G.sum {
        "Home" : G.noArgs
      , "Login" : path "login" G.noArgs
    }
