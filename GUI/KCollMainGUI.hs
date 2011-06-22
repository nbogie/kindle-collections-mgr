module GUI.KCollMainGUI where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import FileUtils
import Types
import Parser (encodeToString, parseCollectionsJSON)
import Data.List (isInfixOf)
import Data.Char (toUpper)
import Data.Text (pack)
import qualified Data.Map as M (empty)
import Data.Aeson.Types (Result(..))
import Data.IORef
import qualified Text.Show.Pretty as Pr

main :: FilePath -> IO ()
main fpath = do 
  
  putStrLn $ "Will read ui from " ++ fpath
  initGUI
  Just xml <- xmlNew fpath
  window         <-  xmlGetWidget  xml  castToWindow    "window1"
  textView       <-  xmlGetWidget  xml  castToTextView  "textview1"
  textViewLog    <-  xmlGetWidget  xml  castToTextView  "textviewLog"
  button         <-  xmlGetWidget  xml  castToButton    "buttonSave"
  buttonSearch   <-  xmlGetWidget  xml  castToButton    "buttonSearch"
  buttonAdd      <-  xmlGetWidget  xml  castToButton    "buttonAdd"
  entrySearch    <-  xmlGetWidget  xml  castToEntry     "entrySearch"
  entryCollName  <-  xmlGetWidget  xml  castToEntry     "entryCollectionName"

  let collPath = "/media/Kindle/system/collections.json"
  Success kcoll <- parseCollectionsJSON collPath -- TODO: handle json parse fail
  kref <- newIORef kcoll

  buttonSearch `onClicked` (doSearch entrySearch textView)
  buttonAdd `onClicked` (doAdd kref entrySearch entryCollName textViewLog)
  button `onClicked` (doSave kref textViewLog)
  widgetShowAll window
  print kcoll

  window `onDestroy` mainQuit
  mainGUI
  putStrLn "done"

tvSetText tv text = do
  b <- textViewGetBuffer tv 
  textBufferSetText b $ text

doAdd kref e eCollName tvLog = do
  currentKC <- readIORef kref
  sought <- entryGetText e
  cName <- entryGetText eCollName
  ks <- searchInner sought
  print $ ("Will add to collection: ", cName, length ks)
  print ks
  let k = addTo ks (pack cName) currentKC
  writeIORef kref k
  let json = encodeToString k
  let kfdisplay = unlines $ (map show ks)
  let kcdisplay = Pr.ppShow k
  tvSetText tvLog kcdisplay -- json

doSearch :: (TextViewClass v, EntryClass e) => e -> v -> IO ()
doSearch e tv = do
  sought <- entryGetText e
  ks <- searchInner sought
  tvSetText tv $ unlines $ map show ks

searchInner sought = do
  putStrLn  $ "Searching for " ++ sought
  allKs <- getKFilesFrom "/media/Kindle/"
  return $ filter (comp sought) allKs
    where 
      comp s (KFile p h) = map toUpper s `isInfixOf` map toUpper p

  
doSave ::  (TextViewClass t) => IORef KindleCollections -> t -> IO ()
doSave kref tvLog = do
  putStrLn "Saving json"
  kc <- readIORef kref
  let json = encodeToString kc
  tvSetText tvLog json
  
