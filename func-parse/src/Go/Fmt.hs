{-# LANGUAGE ForeignFunctionInterface #-}
module Go.Fmt (
    gofmt
) where

import Data.List (isPrefixOf)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (CString, peekCString, newCString)

foreign import ccall unsafe "gofmt" go_fmt :: CString -> CString

gofmt :: String -> Either String String
gofmt src = unsafePerformIO $ do
    s   <- newCString src
    res <- peekCString $ go_fmt s
    if isPrefixOf "!ERR: " res
        then return . Left $ drop 6 res
        else return $ Right res
