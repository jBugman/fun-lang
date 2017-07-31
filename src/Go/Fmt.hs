module Go.Fmt (gofmt) where

import ClassyPrelude
import Data.Maybe       (fromJust)
import Data.Text        (strip)
import System.Exit      (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process   (readProcessWithExitCode)
import Text.Read.Extra  (readMaybe)

import Fun.Errors (Error (..), Pos (..))


gofmt :: Text -> Either Error Text
gofmt src = unsafePerformIO $ do
    (exitcode, output, errors) <- readProcessWithExitCode "gofmt" [] (unpack src)
    return $ case exitcode of
        ExitSuccess   -> Right $ pack output
        ExitFailure _ -> Left  $ parseError errors

parseError :: String -> Error
parseError err = GoError pos txt
    where
        pos = Pos <$> line <*> col
        txt = strip rest
        (col, rest) = getInt e'
        (line, e')  = getInt e
        e = firstError err


firstError :: String -> Text
firstError e
    = pack
    $ fromJust -- Let it crash if it fails
    $ stripPrefix "<standard input>:"
    $ headEx   -- Let it crash if it fails
    $ lines e


getInt :: Text -> (Maybe Int, Text)
getInt err = (int , rest)
    where
        int       = readMaybe (unpack d)
        (d, rest) = dropWhile isColon <$> break isColon err
        isColon   = (==':')
