import OpenSSL.Digest
main = makeSHA1Digest testStr

testStr = "/mnt/us/documents/hackermonthly-issue1.pdf"

makeSHA1Digest ::  String -> IO String
makeSHA1Digest inputStr = fmap (>>=toHex) (digest SHA1 (toWord inputStr))
  where toWord = map (toEnum . fromEnum)
