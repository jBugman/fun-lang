module Main where

import Fun.Parser
import Text.Parsec (parse)
import System.Exit (die)

main :: IO ()
main = do
    source <- getContents
    case parse funImport "" source of
        Right result -> putStr $ show result
        Left err -> die $ show err
