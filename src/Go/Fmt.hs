module Go.Fmt (gofmt) where

import ClassyPrelude
import Data.Maybe       (fromJust)
import Data.Text        (stripEnd)
import System.Exit      (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process   (readProcessWithExitCode)

import Fun.Errors (Error (..))


gofmt :: Text -> Either Error Text
gofmt src = unsafePerformIO $ do
    (exitcode, output, errors) <- readProcessWithExitCode "gofmt" [] (unpack src)
    return $ case exitcode of
        ExitSuccess   -> Right $ pack output
        ExitFailure _ -> Left . GoError
            $ fromJust -- We want exception there if something go wrong
            $ stripPrefix errPrefix
            $ stripEnd
            $ pack errors

errPrefix :: Text
errPrefix = "<standard input>:"
