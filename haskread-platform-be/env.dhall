let DBConfig = {
        host : Text,
        port : Natural,
        dbName : Text,
        dbUserName : Text,
        dbPassword : Text
    }

let OAuth2Config = {
    clientID : Text,
    clientSecret : Text,
}

let Env = {
    dbConfig : DBConfig,
    logLevel : Text,
    applicationPort : Natural,
    mailAPIToken : Text,
    mailFromEmail : Text,
    oauth2Config : OAuth2Config,
    tokenExpiryTime : Natural, -- Seconds
    environment_ : Text,
    ip_ : Text
}

let dbConfig : DBConfig = {
    host = "localhost",
    port = 5433,
    dbName = "haskread_local_db",
    dbUserName = "tushar",
    dbPassword = "1234"
}

let oauth2Config : OAuth2Config = {
    clientID  =  env:GoogleOAuth2ClientID as Text
  , clientSecret = env:GoogleOAuth2ClientSecret as Text
}

let env : Env = {
    dbConfig = dbConfig,
    logLevel = "LevelDebug",
    applicationPort = 8085,
    mailAPIToken = env:MailAPIToken as Text,
    mailFromEmail = env:MailFromEmail as Text,
    oauth2Config = oauth2Config,
    tokenExpiryTime = 3600, -- Seconds
    environment_ = "local",
    ip_ = "127.0.0.1"
}

in env
