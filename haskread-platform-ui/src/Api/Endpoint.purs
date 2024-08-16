module Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', prefix, root)
import Routing.Duplex.Generic (noArgs,sum)
import Routing.Duplex.Generic.Syntax ((/))


data Endpoint
    = Login
      | Threads

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
    { "Login" : "users" / "auth" / "login" / noArgs
    , "Threads" : "threads" / noArgs
    }