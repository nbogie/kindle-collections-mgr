import SHAUtils (makeSHA1Digest)
import Parser hiding (main)
import FileUtils

import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Environment (getArgs)
import qualified Data.Map as M
import Data.List (find)
import Data.Text (Text(..), pack, unpack)

main ::IO ()
main = do
  args <- getArgs
  files <- case args of
      [root] -> processContents root
      _      -> error usage
  return ()
data AnnotCollection = AnnotCollection Text [(KHash, Maybe FilePath)] Integer deriving (Show)

type KHash = Text
data KFile = KFile String KHash deriving (Show)

processContents root = do
  let collPath = root ++ "system/collections.json"
  Success kcoll <- parseCollectionsJSON collPath -- TODO: handle json parse fail
  print kcoll
  docs <- collectDocs $ root ++ "documents"
  digests <- mapM makeHashForFile docs
  let kfiles = (zipWith KFile) docs digests
  print $ annotateAll kfiles kcoll
  -- mapM (showInColls kcoll) digests

showInColls :: KindleCollections -> String -> IO ()
showInColls kcolls sought = do
  putStrLn $ "Seeking " ++ show sought


annotateAll :: [KFile] -> KindleCollections -> [AnnotCollection]
annotateAll files (KindleCollections cs) = 
  let csPairs = M.assocs cs
  in map (annotate files) csPairs

annotate :: [KFile] -> (Text, Collection) -> AnnotCollection
annotate files (name, (Collection hashes timestamp)) = 
  AnnotCollection name (map (annotateHash files) hashes) timestamp

annotateHash :: [KFile] -> KHash -> (KHash, Maybe FilePath)
annotateHash files h =
  case find (match h) files of
    Just (KFile p h) -> (h, Just p)
    Nothing -> (h, Nothing)

match :: KHash -> KFile -> Bool
match givenHash (KFile path khash) = "*" ++(unpack khash) == unpack givenHash

usage :: String
usage = "prog path_to_kindle_docs"
