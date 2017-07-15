module Main where

import ClassyPrelude       hiding (writeFile)
import Options.Applicative (argument, execParser, help, info, metavar, short, str, switch)
import System.Exit         (die)
import System.FilePath     (replaceExtension)

import Fun (translate, unError)


data Options = Options
    { writeFile :: Bool
    , filepath  :: FilePath
    }

main :: IO ()
main = execParser opts >>= run where
    opts = info parser mempty
    parser = Options <$> switch ( short 'f' <> help "Write output to file <filename.go>" )
                     <*> argument str (metavar "<filename.fun>")


run :: Options -> IO ()
run = trans

trans :: Options -> IO ()
trans opts = do
    let fp = filepath opts
    source <- readFileUtf8 fp
    let name = replaceExtension fp ".go"
    let output = if writeFile opts then writeFileUtf8 name else putStrLn
    either (die . unError) output $ translate source
