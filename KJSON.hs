{-# LANGUAGE OverloadedStrings #-} 
module KJSON where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec (parse, Result(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as M
import qualified Data.Text as X

import Types

test_file = "input/kindle_collection_short.json"
-- test_file = "input/gen.json"

encodeToString = BSL.unpack . encode

main ::IO ()
main = do 
  result <- (fmap parseFromString $ readFile test_file)
  case result of
    Success kc -> do
      let generated = encodeToString kc
      putStrLn generated 
      case parseFromString generated of
        Success kAgain -> putStrLn "Reparsed generated json ok."
        Error e -> error $ "Couldn't parse generated json" ++ e
    Error e -> error $ "Error parsing read json: " ++ e

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
  toJSON (KindleCollections foo) = toJSON foo

instance ToJSON Collection where
  toJSON (Collection items lastAccess) = 
    object [  "items" .= (map star items)
            , "lastAccess" .= lastAccess ]
    where 
      star h = X.pack $ starStr (X.unpack h)
      starStr h@('*':_) = h
      starStr h@('#':_) = h
      starStr h         = '*':h


parseFromString :: String -> T.Result KindleCollections
parseFromString s = 
  let bs = BS.pack s
  in case parse json bs of
       Done _rest result -> T.parse parseJSON result
       Fail rest ctxts err -> 
         Error $ "JSON parse error: " ++ err ++ ", contexts: " 
                 ++ show ctxts  ++ ", rest: " ++ BS.unpack rest
       Partial _           -> 
         Error "JSON parse error.  Unexpected partial."

