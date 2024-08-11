let DBConfig = {
        host : Text,
        port : Natural,
        dbName : Text,
        dbUserName : Text,
        dbPassword : Text
    }

let OAuth2Config = {
    clientID : Text,
    clientSecret : Text
}

let Env = {
    dbConfig : DBConfig,
    logFilePath : Text,
    logLevel : Text,
    fileUploadPath : Text,
    applicationPort : Natural,
    mailAPIToken : Text,
    mailFromEmail : Text,
    oauth2Config : OAuth2Config
}

let dbConfig : DBConfig = {
    host = "localhost",
    port = 5434,
    dbName = "haskread_test_db",
    dbUserName = "tushar",
    dbPassword = "1234"
}


let oauth2Config : OAuth2Config = {
    clientID  =  env:GoogleOAuth2ClientID as Text
    , clientSecret = env:GoogleOAuth2ClientSecret as Text
}

let env : Env = {
    dbConfig = dbConfig,
    logFilePath = "./.logs/dev_logs.txt",
    logLevel = "LevelDebug",
    fileUploadPath = "./file-upload",
    applicationPort = 8085,
    mailAPIToken = env:MailAPIToken as Text,
    mailFromEmail = env:MailFromEmail as Text,
    oauth2Config = oauth2Config
}

in env
