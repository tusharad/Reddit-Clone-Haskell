let Env = {
    bucketName : Text
  , uiObjectPath : Text
  , beObjectPath : Text
  , uiLocation : Text
  , beLocation : Text
  , uiServiceName : Text
  , beServiceName : Text
  , dbDockerName : Text
  , downloadsPath : Text
  , bearerToken : Text
}

let env : Env = {
    bucketName = "haskread_ci_storage"
  , uiObjectPath = "artifacts/ui-binary/haskread-platform-ui-exe"
  , beObjectPath = "artifacts/be-binary/haskread-platform-be-exe"
  , uiLocation = "/home/user/Documents/haskread-platform-ui-exe"
  , beLocation = "/home/user/Documents/haskread-platform-be-exe"
  , uiServiceName = "ui.service"
  , beServiceName = "be.serive"
  , dbDockerName = "haskread_prod_pg_db"
  , downloadsPath = "/home/user/Documents/HaskRead-Prod"
  , bearerToken = "1234"
}

in env
