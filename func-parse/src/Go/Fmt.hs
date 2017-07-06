{-# LANGUAGE ForeignFunctionInterface #-}
module Go.Fmt (
    gofmt
) where

import Data.List        (isPrefixOf)
import Foreign.C.String (CString, newCString, peekCString)
import System.IO.Unsafe (unsafePerformIO)


foreign import ccall unsafe "gofmt" go_fmt :: CString -> CString

gofmt :: String -> Either String String
gofmt src = unsafePerformIO $ do
    s   <- newCString src
    res <- peekCString $ go_fmt s
    if "!ERR: " `isPrefixOf` res
        then return . Left $ drop 6 res
        else return $ Right res
