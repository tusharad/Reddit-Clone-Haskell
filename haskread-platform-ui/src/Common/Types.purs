module Common.Types
  ( BaseURL(..)
  , Endpoint(..)
  , MyRoute(..)
  , PaginatedArray
  , RequestMethod(..)
  , RequestOptions
  , Thread
  , Token(..)
  , endpointCodec
  , myRoute
  , threadCodec
  , threadsCodec
  )
  where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex.Generic as G
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', int, path, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CAR
import Data.Codec ((>~>))
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC

newtype BaseURL = BaseURL String

data Endpoint = Threads 
derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum { "Threads" : "api" / "v1" / "thread" / "all" / noArgs }
-- root $ prefix "api" $ sum {
--         "Threads" : "threads" / noArgs
--     }

data RequestMethod = 
    Get | Post (Maybe Json) | Put (Maybe Json) | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

data MyRoute =
      Home
    | Login

derive instance genericRoute :: Generic MyRoute _
derive instance eqRoute :: Eq MyRoute

newtype Token = Token String

derive instance eqToken :: Eq Token
instance showToken :: Show Token where
    show (Token _) = "TOKEN"

myRoute :: RouteDuplex' MyRoute
myRoute = root $ G.sum {
        "Home" : G.noArgs
      , "Login" : path "login" G.noArgs
    }

type PaginatedArray a =
  { total :: Int
  , body :: Array a
  }

-- Thread
type Thread = {
    title :: String,
    description :: Maybe String
  }

threadCodec :: JsonCodec Thread
threadCodec =
    CAR.object "Thread" {
        title : CA.string,
        description : CAC.maybe CA.string
    }

threadsCodec :: JsonCodec (PaginatedArray Thread)
threadsCodec = 
    CAM.renameField "threads" "body"
    >~> CAM.renameField "threadsCount" "total"
    >~> codec
  where
  codec =
    CAR.object "PaginatedArray Thread"
      { body: CA.array threadCodec
      , total: CA.int
      }