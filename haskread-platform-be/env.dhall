let DBConfig = {
        host : Text,
        port : Natural,
        dbName : Text,
        dbUserName : Text,
        dbPassword : Text
    }

let Env = {
    dbConfig : DBConfig,
    logFilePath : Text,
    logLevel : Text,
    fileUploadPath : Text,
    applicationPort : Natural,
    mailAPIToken : Text,
    mailFromEmail : Text
}

let dbConfig : DBConfig = {
    host = "localhost",
    port = 5433,
    dbName = "haskread_local_db",
    dbUserName = "tushar",
    dbPassword = "1234"
}

let env : Env = {
    dbConfig = dbConfig,
    logFilePath = "/home/user/haskell/Reddit-Clone-Haskell/haskread-platform-be/.logs/dev_logs.txt",
    logLevel = "LevelDebug",
    fileUploadPath = "/home/user/haskell/Reddit-Clone-Haskell/haskread-platform-be/file-upload",
    applicationPort = 8085,
    mailAPIToken = env:MailAPIToken as Text,
    mailFromEmail = env:MailFromEmail as Text,
}

in env
