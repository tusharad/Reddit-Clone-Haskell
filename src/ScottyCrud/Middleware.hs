module ScottyCrud.Middleware where
import Network.Wai

checkRouteMiddleware :: Application -> Application 
checkRouteMiddleware app req res = do
  print ("got hit:" , rawPathInfo req, " Method type: " ,requestMethod req)
  app req res

