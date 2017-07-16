module Main where

import Paths_language_fun (version)

import ClassyPrelude       hiding (writeFile)
import Data.Text           (replace)
import Data.Text.IO        (hPutStr)
import Data.Version        (showVersion)
import Options.Applicative (Parser, argument, command, customExecParser, defaultPrefs, help, helper,
                            idm, info, infoOption, long, metavar, prefShowHelpOnEmpty, progDesc,
                            short, str, subparser, switch)
import System.Exit         (ExitCode (..), die)
import System.FilePath     (replaceExtension)
import System.IO           (hFlush, stderr)
import System.IO.Temp      (withSystemTempFile)
import System.Process      (readProcessWithExitCode)

import Fun        (translate)
import Fun.Errors (unError)


main :: IO ()
main = join $ customExecParser prefs argparse
    where
        argparse = info (commandDispatcher <**> versioner <**> helper) idm
        prefs    = defaultPrefs { prefShowHelpOnEmpty = True }

versioner :: Parser (a -> a)
versioner = infoOption
    ("Fun compiler v" <> showVersion version)
    (long "version" <> help "Show version")


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

type WriteFile = Bool

data TranslateOptions = TranslateOptions WriteFile FilePath

transCommand :: Parser TranslateOptions
transCommand = TranslateOptions
    <$> switch ( short 'f' <> help "Write output to file <filename.go>" )
    <*> argument str (metavar "<filename.fun>")

doTrans :: TranslateOptions -> IO ()
doTrans (TranslateOptions toFile filePath) = translateFile outputResult filePath
    where
        outputResult = if toFile
            then writeFileUtf8 (replaceExtension filePath ".go")
            else putStrLn

translateFile :: (Text -> IO ()) -> FilePath -> IO ()
translateFile successPath filePath = do
    source <- readFileUtf8 filePath
    either errPath successPath $ translate source
    where
        errPath = die . unpack . unError


-- Run --

newtype RunOptions = RunOptions FilePath

runCommand :: Parser RunOptions
runCommand = RunOptions <$> argument str (metavar "<filename.fun>")

doRun :: RunOptions -> IO ()
doRun (RunOptions filePath) = translateFile gorunTempfile filePath where
    gorunTempfile :: Text -> IO ()
    gorunTempfile src = withSystemTempFile "funRun.go" (action src)

    action :: Text -> FilePath -> Handle -> IO ()
    action src path hdl = do
        hPutStr hdl src
        hFlush hdl
        (exitcode, output, errors) <- readProcessWithExitCode "go" ["run", path] []
        case exitcode of
            ExitSuccess   -> putStr (pack output)
            ExitFailure _ -> hPutStr stderr $ replaceFileName errors
        where
            replaceFileName s = replace (pack path) "~tmp.go" (pack s)
