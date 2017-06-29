module Main where

import System.Exit (die)
import qualified Fun.Main as F

main :: IO ()
main = do
    source <- getContents
    case F.parsePackage source of
        Left  err -> die (F.prettyError err)
        Right p   -> F.printAsJSON p
