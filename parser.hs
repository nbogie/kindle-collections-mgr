{-# LANGUAGE OverloadedStrings #-} 

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec (parse, Result(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

data KindleCollections = KindleCollections Collection deriving (Show)
data Collection = Collection [String] Integer deriving (Show)

test_file = "kindle_collection_short.json"
-- test_file = "collection.json"

main ::IO ()
main = do 
  (fmap parseFromString $ readFile test_file) >>= print


instance FromJSON KindleCollections where
  parseJSON (Object v) = KindleCollections <$>
                          v .: "foo"
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

