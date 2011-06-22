module Types where
import Data.Text (Text(..), pack, unpack)

import Data.Map as M

type KHash = Text

data KFile = 
  KFile String KHash 
  deriving (Show)

data KindleCollections = 
  KindleCollections CollMap 
  deriving (Show)

type CollMap = Map Text Collection

data Collection = 
  Collection [Text] Integer 
  deriving (Show)

data AnnotCollection = 
  AnnotCollection Text [(KHash, Maybe FilePath)] Integer 
  deriving (Show)
