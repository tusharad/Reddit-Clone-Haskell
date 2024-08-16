module Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug
import Data.Either (note)

data Route = Home 
            | Login
            | Signup 

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
    show Home = "Home" 
    show Login = "Home" 
    show Signup = "Home" 

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum {
    "Home" :  noArgs,
    "Login" : "login" / noArgs,
    "Signup" : "signup" / noArgs   
 }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")