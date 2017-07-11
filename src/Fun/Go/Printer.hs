module Fun.Go.Printer (
    printPretty, print, SyntaxError(..)
) where

import           ClassyPrelude           hiding (print)
import           Data.Either             (partitionEithers)
import           Data.Either.Combinators (mapBoth)
import qualified Data.Text               as T
import qualified Data.Text.Format        as F
import qualified Data.Text.Format.Params as F

import qualified Fun.SExpression as S
import           Go.Fmt


printPretty :: S.Expression -> Either SyntaxError Text
printPretty s = case print s of
    Right txt -> mapBoth SyntaxError id (gofmt txt)
    err       -> err


print :: S.Expression -> Either SyntaxError Text
-- empty
print (S.Exp []) = syntaxErr "empty expression"

-- literal
print (S.Atom s) = Right s

-- package
print (S.Exp ("package":name:topLevels)) = case partitionEithers $ fmap print topLevels of
    (err:_ , _) -> Left err
    ([], txts)  -> Right $ printf "package {}\n\n{}" (name, ointercalate "\n\n" txts)

-- import
print (S.Exp ["import", path]) = Right $ printf "import {}" (F.Only path)
print (S.Exp ["import", path, alias]) = Right $ printf "import {} {}" (unquote alias, path)

-- func
print (S.Exp ["func", S.Atom name, body]) = printSubtree "func {}() {\n{}\n}" name body

-- assignment
print (S.Exp ["set", S.Atom name, body]) = printSubtree "{} = {}" name body

-- function call
print (S.Exp (S.Atom f : args)) = funcCall f args

-- operators
print (S.Exp [S.Op op, lhs, rhs]) = case (print lhs, print rhs) of
    (Left e, _)          -> Left e
    (_, Left e)          -> Left e
    (Right lt, Right rt) -> Right $ printf "{} {} {}" (lt, o, rt)
        where
            o = if op == "=" then "==" else op

-- catch-all todo case
print s = syntaxErr $ printf "not supported yet: {}" (F.Only s)


-- Function call printer
funcCall :: Text -> [S.Expression] -> Either SyntaxError Text
funcCall name args = case partitionEithers $ fmap print args of
    (err:_ , _) -> Left err
    ([], txts)  -> Right $ printf "{}({})" (name, ointercalate ", " txts)


-- Types --

newtype SyntaxError = SyntaxError Text deriving (Eq, Show)

-- Utils --

printSubtree :: F.Format -> Text -> S.Expression -> Either SyntaxError Text
printSubtree fmt x y = mapBoth id (\s -> printf fmt (x, s)) (print y)

printf :: F.Params ps => F.Format -> ps -> Text
printf fmt ps = toStrict $ F.format fmt ps

syntaxErr :: Text -> Either SyntaxError Text
syntaxErr = Left . SyntaxError

unquote :: S.Expression -> Maybe Text
unquote (S.Atom t) = Just $ T.dropAround (== '\"') t
unquote _          = Nothing
