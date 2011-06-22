import SHAUtils (makeSHA1Digest)
import Parser hiding (main)
import FileUtils (makeHashForFile, collectDocs, makeKFile, getKFilesFrom)
import Types

import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Environment (getArgs)
import qualified Data.Map as M
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
  kfiles <- getKFilesFrom root
  print $ annotateAll kfiles kcoll
  -- mapM (showInColls kcoll) digests

usage :: String
usage = "prog path_to_kindle_docs"

test = do
  fs <- mapM makeKFile ["fiction/foo.pdf", "science/bar.mobi"]
  let k = createCollection newCollection (pack "fiction")
  let k' = addTo fs (pack "science") k
  putStrLn $ encodeToString k'
