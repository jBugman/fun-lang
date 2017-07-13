module Fun.Go.Printer (
    printPretty, print, SyntaxError(..)
) where

import           ClassyPrelude                hiding (print)
import           Data.Either                  (partitionEithers)
import           Data.Either.Combinators      (mapBoth)
import           Data.SCargot.Repr.WellFormed
import           Data.Text                    (dropAround)
import qualified Data.Text.Format             as F
import qualified Data.Text.Format.Params      as F

import Fun.SExpression (Atom (..), Expression, Lit (..))
import Go.Fmt


printPretty :: Expression -> Either SyntaxError Text
printPretty s = case print s of
    Right txt -> mapBoth SyntaxError id (gofmt txt)
    err       -> err


print :: Expression -> Either SyntaxError Text
-- empty
print Nil = syntaxErr "empty expression"

-- ident
print (A (Ident x)) = Right x

-- literal
print (A (Lit x)) = Right $ printf "{}" (F.Only x)

-- package
print (L (A (Ident "package") : A (Ident name) : topLevels)) = case partitionEithers $ fmap print topLevels of
    (err:_ , _) -> Left err
    ([], txts)  -> Right $ printf "package {}\n\n{}" (name, ointercalate "\n\n" txts)

-- import
print (L [A (Ident "import"), A (Lit (S path))]) = Right $ printf "import {}" (F.Only path)  -- FIXME:
print (L [A (Ident "import"), A (Lit (S path)), A (Lit (S alias))]) =
    Right $ printf "import {} {}" (unquote alias, path)  -- FIXME:

-- func
print (L [A (Ident "func"), A (Ident name), body]) = printSubtree "func {}() {\n{}\n}" name body

-- assignment
print (L [A (Ident "set"), A (Ident name), body]) = printSubtree "{} = {}" name body

-- function call
print (L (A (Ident f) : args)) = funcCall f args

-- operators
print (L [A (Op op), lhs, rhs]) = case (print lhs, print rhs) of
    (Left e, _)          -> Left e
    (_, Left e)          -> Left e
    (Right lt, Right rt) -> Right $ printf "{} {} {}" (lt, o, rt)
        where
            o = if op == "=" then "==" else op

-- catch-all todo case
print s = syntaxErr $ printf "not supported yet: {}" (F.Only s)


-- Function call printer
funcCall :: Text -> [Expression] -> Either SyntaxError Text
funcCall name args = case partitionEithers $ fmap print args of
    (err:_ , _) -> Left err
    ([], txts)  -> Right $ printf "{}({})" (name, ointercalate ", " txts)


-- Types --

newtype SyntaxError = SyntaxError Text deriving (Eq, Show)

-- Utils --

printSubtree :: F.Format -> Text -> Expression -> Either SyntaxError Text
printSubtree fmt x y = mapBoth id (\s -> printf fmt (x, s)) (print y)

printf :: F.Params ps => F.Format -> ps -> Text
printf fmt ps = toStrict $ F.format fmt ps

syntaxErr :: Text -> Either SyntaxError Text
syntaxErr = Left . SyntaxError

unquote :: Text -> Text
unquote = dropAround (== '\"')
