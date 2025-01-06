module Manager.Common.Utils (genAccessToken, addExecutableFilePermission) where

import qualified Data.ByteString.Char8 as BS
import System.Process 
import System.Posix.Files
import Control.Exception (try)

genAccessToken :: IO (Either String BS.ByteString)
genAccessToken = do 
  eRes <- try $ readProcess "gcloud" (words "auth print-access-token") []
  case eRes of
    Right s -> pure . Right $ BS.pack (init s)
    Left err -> pure . Left $ show (err :: IOError)

addExecutableFilePermission :: FilePath -> IO ()
addExecutableFilePermission saveLocation = do
  fStatus <- getFileStatus saveLocation
  setFileMode saveLocation (fileMode fStatus `unionFileModes` ownerExecuteMode)

