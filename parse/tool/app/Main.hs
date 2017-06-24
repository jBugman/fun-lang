module Main where

import Fun.Parser

main :: IO ()
main = do
    source <- getContents
    print $ parse funImport "stdin" source
