module Main where

import ClassyPrelude
import System.Exit     (die)
import System.FilePath (replaceExtension)

import Fun (SyntaxError (..), translate)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [ filename ] -> trans (unpack filename)
        _            -> die usage


usage :: String
usage = intercalate "\n"
    [ "Usage:"
    , "<filename.fun> — Translate to Go"
    ]

trans :: FilePath -> IO ()
trans fp = do
    source <- readFileUtf8 fp
    let name = replaceExtension fp ".go"
    case translate source of
        Left  (SyntaxError err) -> die (unpack err)
        Right txt               -> writeFileUtf8 name txt
