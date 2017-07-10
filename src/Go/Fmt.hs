{-# LANGUAGE ForeignFunctionInterface #-}
module Go.Fmt (gofmt) where

import ClassyPrelude
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)


foreign import ccall unsafe "gofmt" go_fmt :: CString -> CString

gofmt :: Text -> Either Text Text
gofmt src = unsafePerformIO $ do
    s   <- newCString $ toList src
    res <- peekCString $ go_fmt s
    let txt = pack res
    return $ case stripPrefix errPrefix txt of
        Just err -> Left err
        Nothing  -> Right txt

errPrefix :: Text
errPrefix = "!ERR: "
