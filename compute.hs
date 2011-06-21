import Crypto.Hash.SHA1
import qualified Data.ByteString.Char8 as BS

main = interact hashit

hashit :: String -> String
hashit s = BS.unpack $ hash $ BS.pack s
  
