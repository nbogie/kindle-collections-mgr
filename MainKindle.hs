import SHAUtils (makeSHA1Digest)
import Parser hiding (main)
import FileUtils (makeHashForFile, collectDocs, makeKFile)
import Types

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

processContents root = do
  let collPath = root ++ "system/collections.json"
  Success kcoll <- parseCollectionsJSON collPath -- TODO: handle json parse fail
  print kcoll
  docs <- collectDocs $ root ++ "documents"
  digests <- mapM makeHashForFile docs
  let kfiles = (zipWith KFile) docs digests
  print $ annotateAll kfiles kcoll
  -- mapM (showInColls kcoll) digests

newCollection :: KindleCollections
newCollection = KindleCollections M.empty

-- will add a new collection if one doesn't already exist
addTo :: [KFile] -> Text  -> KindleCollections -> KindleCollections
addTo fs cname (KindleCollections cmap) = 
  KindleCollections newMap
    where 
      newMap :: CollMap
      newMap = M.insert cname newColl cmap
      newColl = addToCollection c newHashes
      c = case M.lookup cname cmap of
        Just coll -> coll
        Nothing   -> Collection [] neverTime
      newHashes = map getH fs
      getH (KFile p h) = h

createCollection :: KindleCollections -> Text -> KindleCollections
createCollection kc@(KindleCollections cmap) cname =
  case M.lookup cname cmap of
    Just existing -> kc
    Nothing -> KindleCollections $ M.insert cname (Collection [] neverTime) cmap

neverTime = 0 -- jan 1970

addToCollection :: Collection -> [KHash] -> Collection
addToCollection (Collection hs la) hsNew = Collection (hs ++ hsNew) la

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

test = do
  fs <- mapM makeKFile ["fiction/foo.pdf", "science/bar.mobi"]
  let k = createCollection newCollection (pack "fiction")
  let k' = addTo fs (pack "science") k
  putStrLn $ encodeToString k'
