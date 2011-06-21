{-# LANGUAGE OverloadedStrings #-} 
module Parser where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec (parse, Result(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map as M
import Data.Text (Text(..))

type CollMap = Map Text Collection
data KindleCollections = KindleCollections CollMap deriving (Show)
data Collection = Collection [Text] Integer deriving (Show)

test_file = "input/kindle_collection_short.json"

main ::IO ()
main = do 
  (fmap parseFromString $ readFile test_file) >>= print
  result <- (fmap parseFromString $ readFile test_file)
  case result of
    Success kc -> putStrLn "" -- $ BSL.unpack $ encode kc
    Error e -> putStrLn $ "err " ++ show e

parseCollectionsJSON :: FilePath -> IO (T.Result KindleCollections)
parseCollectionsJSON p = do
  fmap parseFromString (readFile p)

instance FromJSON KindleCollections where
  parseJSON ob@(Object v) = KindleCollections <$> parseJSON ob
  parseJSON _          = mzero

instance FromJSON Collection where
  parseJSON (Object v) = Collection <$>
                          v .: "items" <*> v .: "lastAccess"
  parseJSON _          = mzero

instance ToJSON KindleCollections where
  toJSON (KindleCollections foo) = object ["x" .= foo]

instance ToJSON Collection where
  toJSON (Collection items lastAccess) = object ["items" .= items]

parseFromString :: String -> T.Result KindleCollections
parseFromString s = 
  let bs = BS.pack s
  in case parse json bs of
       Done _rest result -> T.parse parseJSON result
       Fail rest ctxts err -> Error $ "JSON parse error: " ++ err ++ ", contexts: " ++ show ctxts  ++ ", rest: " ++ BS.unpack rest
       Partial _           -> Error "JSON parse error.  Unexpected partial."
