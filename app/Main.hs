module Main where

import ClassyPrelude
import Options.Applicative (argument, execParser, info, metavar, str)
import System.Exit         (die)
import System.FilePath     (replaceExtension)

import Fun (SyntaxError (..), translate)


newtype Options = Options
    { filename  :: FilePath
    }

main :: IO ()
main = execParser opts >>= run where
    opts = info parser mempty
    parser = Options <$> argument str (metavar "<filename.fun>")

run :: Options -> IO ()
run opts = trans (filename opts)

trans :: FilePath -> IO ()
trans fp = do
    source <- readFileUtf8 fp
    let name = replaceExtension fp ".go"
    case translate source of
        Left  (SyntaxError err) -> die (unpack err)
        Right txt               -> writeFileUtf8 name txt
