{-# LANGUAGE OverloadedStrings #-}

module Manager.Storage (downloadObject) where

import Control.Exception (try)
import qualified Data.ByteString as BS
import Manager.Common.Types
import Manager.Common.Utils
import Network.HTTP.Req

downloadObject :: BuckectName -> ObjectName -> FilePath -> IO (Either String ())
downloadObject bucketName objectName saveLocation = do
    eAccessToken <- genAccessToken
    case eAccessToken of
        Left e -> do
            let errMsg = "Couldn't generate Access token: " <> show e
            pure $ Left errMsg
        Right accessToken -> do
            eRes <- try $ runReq defaultHttpConfig $ do
                req
                    GET
                    ( https
                        "storage.googleapis.com"
                        /: "storage"
                        /: "v1"
                        /: "b"
                        /: bucketName
                        /: "o"
                        /: objectName
                    )
                    NoReqBody
                    bsResponse
                    $ oAuth2Bearer accessToken
                        <> ("alt" =: ("media" :: String))
            case eRes of
                Left e -> do
                    let errMsg =
                            "Something went wrong while downloading object: "
                                <> show (e :: HttpException)
                    pure $ Left errMsg
                Right resp -> do
                    if not $
                        responseStatusCode resp < 299 && responseStatusCode resp > 199
                        then
                            pure $ Left $ "Error: " <> show (responseStatusMessage resp)
                        else do
                            eSaveFile <- try $ BS.writeFile saveLocation (responseBody resp)
                            case eSaveFile of
                                Right _ -> do 
                                  addExecutableFilePermission saveLocation
                                  pure $ Right ()
                                Left err ->
                                    pure $
                                        Left $
                                            "Error while saving file: "
                                                <> show (err :: IOError)
