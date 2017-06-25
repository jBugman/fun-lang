module Main where

import Fun.Parser (prs, funImport)
import System.Exit (die)

main :: IO ()
main = do
    source <- getContents
    case prs funImport source of
        Right result -> putStr $ show result
        Left err -> die $ show err
