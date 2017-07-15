{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import ClassyPrelude       hiding (writeFile)
import Options.Applicative (Parser, argument, command, customExecParser, defaultPrefs, help, helper,
                            idm, info, metavar, prefShowHelpOnEmpty, progDesc, short, str,
                            subparser, switch)
import System.Exit         (die)
import System.FilePath     (replaceExtension)

import Fun (translate, unError)


main :: IO ()
main = do
    let prefs = defaultPrefs { prefShowHelpOnEmpty = True }
    let argParser = info (commands <**> helper) idm
    action <- customExecParser prefs argParser
    case action of
        Translate opts -> doTrans opts
        Run       opts -> doRun opts

data Command
    = Translate TranslateOptions
    | Run       RunOptions

commands :: Parser Command
commands = subparser
    (  command "translate" (info
        ( transCommand <**> helper )
        ( progDesc "Translate Fun source file to Go (default)" ) )
    <> command "run"       (info
        ( runCommand   <**> helper )
        ( progDesc "'go run' translated on the fly Fun source file" ) )
    ) <|> transCommand -- Default command


-- Translate --

data TranslateOptions = TranslateOptions
    { writeFile :: Bool
    , filepath  :: FilePath
    }

transCommand :: Parser Command
transCommand = Translate <$> optParser where
    optParser = TranslateOptions
        <$> switch ( short 'f' <> help "Write output to file <filename.go>" )
        <*> argument str (metavar "<filename.fun>")

doTrans :: TranslateOptions -> IO ()
doTrans opts = do
    let fp = filepath (opts :: TranslateOptions)
    source <- readFileUtf8 fp
    let name = replaceExtension fp ".go"
    let output = if writeFile opts then writeFileUtf8 name else putStrLn
    either (die . unError) output $ translate source


-- Run --

newtype RunOptions = RunOptions { filepath :: FilePath }

runCommand :: Parser Command
runCommand = Run <$> (RunOptions <$> argument str (metavar "<filename.fun>"))

doRun :: RunOptions -> IO ()
doRun _ = putStrLn "test run" -- TODO: implement
