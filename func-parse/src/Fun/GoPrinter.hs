module Fun.GoPrinter (
    print, SyntaxError
) where

import Prelude hiding (print)
import Data.Either (partitionEithers)
import Data.Text (Text, dropAround)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as F
import qualified Data.Text.Format.Params as F

import qualified Fun.Sexp as S


print :: S.Expression -> PrintResult
-- empty
print (S.Exp []) = syntaxErr ("empty expression" :: LT.Text)

-- literal
print (S.Atom s) = Right $ LT.fromStrict s

-- package
print (S.Exp ("package":name:topLevels)) = case partitionEithers $ map print topLevels of
    (err:_ , _) -> Left err
    ([], txts)  -> printf "package {}\n\n{}" (name, LT.intercalate "\n\n" txts)

-- import
print (S.Exp ["import", path]) = printf "import \"{}\"" $ F.Only path
print (S.Exp ["import", path, alias]) = printf "import {} \"{}\"" (unquote alias, path)

-- func
print (S.Exp ["func", name, body]) = case print body of
    Right s -> printf "func {}() {\n{}}" (name, indented s)
    err     -> err

-- operators
print (S.Exp ["=", S.Atom lhs, expr]) = case print expr of
    Right s -> printf "{} = {}" (lhs, s)
    err     -> err

print s = syntaxErr $ F.format "not supported yet: {}" $ F.Only s


-- Types --
newtype SyntaxError = SyntaxError LT.Text deriving (Eq, Show)

type PrintResult = Either SyntaxError LT.Text

-- Utils --

printf :: F.Params ps => F.Format -> ps ->PrintResult
printf fmt ps = Right $ F.format fmt ps

syntaxErr :: LT.Text -> PrintResult
syntaxErr = Left . SyntaxError

unquote :: S.Expression -> Maybe Text
unquote (S.Atom t) = Just $ dropAround (== '\"') t
unquote _ = Nothing

indented :: LT.Text -> LT.Text
indented t = LT.unlines $ map (LT.append "\t") (LT.lines t)
