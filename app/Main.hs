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
main = join $ customExecParser prefs argparse
    where
        argparse = info (commandDispatcher <**> helper) idm
        prefs    = defaultPrefs { prefShowHelpOnEmpty = True }

data Command
    = Translate TranslateOptions
    | Run       RunOptions

commandDispatcher :: Parser (IO ())
commandDispatcher = subparser
    (  command "translate" (info
        ( translator <**> helper )
        ( progDesc "Translate Fun source file to Go (default)" ) )
    <> command "run"       (info
        ( doRun <$> (runCommand <**> helper) )
        ( progDesc "'go run' translated on the fly Fun source file" ) )
    ) <|> translator -- Default command
    where
        translator = doTrans <$> transCommand


-- Translate --

data TranslateOptions = TranslateOptions
    { writeFile :: Bool
    , filepath  :: FilePath
    }

transCommand :: Parser TranslateOptions
transCommand = TranslateOptions
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

runCommand :: Parser RunOptions
runCommand = RunOptions <$> argument str (metavar "<filename.fun>")

doRun :: RunOptions -> IO ()
doRun _ = putStrLn "test run" -- TODO: implement
