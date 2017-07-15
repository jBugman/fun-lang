{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import ClassyPrelude       hiding (writeFile)
import Options.Applicative (Parser, argument, command, execParser, help, helper, info, metavar,
                            short, str, subparser, switch)
import System.Exit         (die)
import System.FilePath     (replaceExtension)

import Fun (translate, unError)


main :: IO ()
main = do
    let argParser = info (commands <**> helper) mempty
    action <- execParser argParser
    case action of
        Translate opts -> trans opts
        Run       _    -> putStrLn "test run"

data Command
    = Translate TranslateOptions
    | Run       RunOptions

commands :: Parser Command
commands = subparser
    (  command "translate" (info (Translate <$> transCommand <**> helper) mempty)
    <> command "run"       (info (Run       <$> runCommand <**> helper) mempty)
    ) <|> (Translate <$> transCommand) -- Default command


-- Translate --

data TranslateOptions = TranslateOptions
    { writeFile :: Bool
    , filepath  :: FilePath
    }

transCommand :: Parser TranslateOptions
transCommand = TranslateOptions
    <$> switch ( short 'f' <> help "Write output to file <filename.go>" )
    <*> argument str (metavar "<filename.fun>")

trans :: TranslateOptions -> IO ()
trans opts = do
    let fp = filepath (opts :: TranslateOptions)
    source <- readFileUtf8 fp
    let name = replaceExtension fp ".go"
    let output = if writeFile opts then writeFileUtf8 name else putStrLn
    either (die . unError) output $ translate source


-- Run --

newtype RunOptions = RunOptions { filepath :: FilePath }

runCommand :: Parser RunOptions
runCommand = RunOptions <$> argument str (metavar "<filename.fun>")
