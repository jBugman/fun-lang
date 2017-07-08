module Fun.Go.Printer (
    printPretty, print, SyntaxError(..)
) where

import Data.Either (partitionEithers)
import Data.Text   (Text, dropAround, pack)
import Prelude     hiding (print)

import qualified Data.Either.Combinators as E
import qualified Data.Text.Format        as F
import qualified Data.Text.Format.Params as F
import qualified Data.Text.Lazy          as LT

import Go.Fmt

import qualified Fun.Sexp as S


printPretty :: S.Expression Text -> PrintResult
printPretty s = case print s of
    Right txt -> E.mapBoth (SyntaxError . pack) LT.pack (gofmt $ LT.unpack txt)
    err       -> err


print :: S.Expression Text -> PrintResult
-- empty
print (S.Exp []) = syntaxErr "empty expression"

-- literal
print (S.Atom s) = Right $ LT.fromStrict s

-- package
print (S.Exp ("package":name:topLevels)) = case partitionEithers $ fmap print topLevels of
    (err:_ , _) -> Left err
    ([], txts)  -> printf "package {}\n\n{}" (name, LT.intercalate "\n\n" txts)

-- import
print (S.Exp ["import", path]) = printf "import \"{}\"" $ F.Only path
print (S.Exp ["import", path, alias]) = printf "import {} \"{}\"" (unquote alias, path)

-- func
print (S.Exp ["func", S.Atom name, body]) = printSubtree "func {}() {\n{}\n}" name body

-- assignment
print (S.Exp ["set", S.Atom name, body]) = printSubtree "{} = {}" name body

-- print placeholder -- TODO: proper desugar
print (S.Exp ("print":args)) = funcCall "print" args

-- operators
print (S.Exp [S.Op op, lhs, rhs]) = case (print lhs, print rhs) of
    (Left e, _)          -> Left e
    (_, Left e)          -> Left e
    (Right lt, Right rt) -> Right $ F.format "{} {} {}" (lt, o, rt)
        where
            o = if op == "=" then "==" else op

-- catch-all todo case
print s = syntaxErr . pack $ "not supported yet: " ++ show s


-- Function call printer
funcCall :: Text -> [S.Expression Text] -> PrintResult
funcCall name args = case partitionEithers $ fmap print args of
    (err:_ , _) -> Left err
    ([], txts)  -> printf "{}({})" (name, LT.intercalate ", " txts)


-- Types --

newtype SyntaxError = SyntaxError Text deriving (Eq, Show)

type PrintResult = Either SyntaxError LT.Text

-- Utils --

printSubtree :: F.Format -> Text -> S.Expression Text -> PrintResult
printSubtree fmt x y = E.mapBoth id (\s -> F.format fmt (x, s)) (print y)

printf :: F.Params ps => F.Format -> ps -> PrintResult
printf fmt ps = Right $ F.format fmt ps

syntaxErr :: Text -> PrintResult
syntaxErr = Left . SyntaxError

unquote :: S.Expression Text -> Maybe Text
unquote (S.Atom t) = Just $ dropAround (== '\"') t
unquote _          = Nothing
