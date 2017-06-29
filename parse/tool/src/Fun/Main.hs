{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Main where

import Prelude hiding (putStr)
import Data.Aeson (encode, ToJSON)
import Data.ByteString.Lazy (ByteString, putStr)

import Fun.Types (Package)
import Fun.Parser (prs, package)


parsePackage :: String -> Either _ Package
parsePackage = prs package

jsonify :: ToJSON t => t -> ByteString
jsonify = encode

toStdout :: ByteString -> IO ()
toStdout = putStr
