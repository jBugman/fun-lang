module Fun.GoPrinter (
    print, SyntaxError
) where

import Prelude hiding (print)
import Data.Either (partitionEithers)
import qualified Data.Either.Combinators as E
import qualified Data.Text as ST
import Data.Text.Lazy
import qualified Data.Text.Format as F
import qualified Data.Text.Format.Params as F

import qualified Fun.Sexp as S


print :: S.Expression -> PrintResult
-- empty
print (S.Exp []) = syntaxErr "empty expression"

-- literal
print (S.Atom s) = Right $ fromStrict s

-- package
print (S.Exp ("package":name:topLevels)) = case partitionEithers $ fmap print topLevels of
    (err:_ , _) -> Left err
    ([], txts)  -> printf "package {}\n\n{}" (name, intercalate "\n\n" txts)

-- import
print (S.Exp ["import", path]) = printf "import \"{}\"" $ F.Only path
print (S.Exp ["import", path, alias]) = printf "import {} \"{}\"" (unquote alias, path)

-- func
print (S.Exp ["func", S.Atom name, body]) = printSubtree "func {}() {\n{}\n}" name body

-- operators
print (S.Exp ["=", S.Atom lhs, expr]) = printSubtree "{} = {}" lhs expr

print s = syntaxErr $ F.format "not supported yet: {}" $ F.Only s


-- Types --

newtype SyntaxError = SyntaxError Text deriving (Eq, Show)

type PrintResult = Either SyntaxError Text

-- Utils --

printSubtree :: F.Format -> ST.Text -> S.Expression -> PrintResult
printSubtree fmt x y = E.mapBoth id (\s -> F.format fmt (x, s)) (print y)

printf :: F.Params ps => F.Format -> ps ->PrintResult
printf fmt ps = Right $ F.format fmt ps

syntaxErr :: Text -> PrintResult
syntaxErr = Left . SyntaxError

unquote :: S.Expression -> Maybe ST.Text
unquote (S.Atom t) = Just $ ST.dropAround (== '\"') t
unquote _ = Nothing
