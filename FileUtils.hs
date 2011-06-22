module FileUtils (collectDocs, makeHashForFile, makeKFile, getKFilesFrom) where

import SHAUtils (makeSHA1Digest)
import KJSON hiding (main)
import Types

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import Control.Monad
import qualified Data.Text  as T
debug :: String -> IO ()
debug s = return () -- putStrLn s

getKFilesFrom :: FilePath -> IO [KFile]
getKFilesFrom root = do
  docs <- collectDocs $ root ++ "documents"
  digests <- mapM makeHashForFile docs
  return $ (zipWith KFile) docs digests

makeKFile :: FilePath -> IO KFile
makeKFile p = do
  h <- makeHashForFile p
  return $ KFile p h

makeHashForFile :: FilePath -> IO T.Text
makeHashForFile p = do
  h <- makeSHA1Digest newPath 
  debug $ "Made hash  " ++ h ++ " for new path: '"++ newPath ++"' old path: " ++ p
  return $ T.pack h
    where newPath = "/mnt/us/documents/" ++ strippedPath
          strippedPath = drop (length toStrip) p
          toStrip = "/media/Kindle/documents/"

collectDocs :: FilePath -> IO [FilePath]
collectDocs path = do
  isf <- doesFileExist path
  isd <- doesDirectoryExist path
  if isf
    then report "file" path >> return [path]
    else 
      if isd
        then report "dir" path >> collectDocsFromDir path
        else do
          report "nonsense" path >> return []

getPlainDirectoryContents :: FilePath -> IO [FilePath]
getPlainDirectoryContents p = do 
  ps <- getDirectoryContents p
  return $ filter notNav ps
    where 
      notNav :: FilePath -> Bool
      notNav path = not (path `elem` ["..", "."])

collectDocsFromDir :: FilePath -> IO [FilePath]
collectDocsFromDir path = do
  paths    <- getPlainDirectoryContents path
  contents <- mapM (collectDocs  . ((path ++ "/") ++ ) ) $ paths
  return $ concat contents
report msg path = return () -- print $ "REPORT: "++ msg ++ ": " ++ path

