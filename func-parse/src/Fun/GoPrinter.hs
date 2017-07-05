module Fun.GoPrinter where

import Prelude hiding (print)
import Data.Either (partitionEithers)
import Data.Text (Text, dropAround)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as F

import qualified Fun.Sexp as S


newtype SyntaxError = SyntaxError LT.Text deriving (Eq, Show)


print :: S.Expression -> Either SyntaxError LT.Text
-- empty
print (S.Exp []) = Left $ SyntaxError ("empty expression" :: LT.Text)

-- literal
print (S.Atom s) = Right $ LT.fromStrict s

-- package
print (S.Exp ("package":name:topLevels)) = case partitionEithers $ map print topLevels of
    (err:_ , _) -> Left err
    ([], txts)   -> Right $ F.format "package {}\n\n{}" (name, LT.intercalate "\n\n" txts)

-- import
print (S.Exp ["import", path]) = Right $ F.format "import \"{}\"" $ F.Only path
print (S.Exp ["import", path, alias]) = Right $ F.format "import {} \"{}\"" (unquote alias, path)

-- func
print (S.Exp ["func", name, body]) = case print body of
    Right s -> Right $ F.format "func {}() {\n{}}" (name, indented s)
    err     -> err

-- operators
print (S.Exp ["=", S.Atom lhs, expr]) = case print expr of
    Right s -> Right $ F.format "{} = {}" (lhs, s)
    err     -> err

print s = Left $ SyntaxError $ F.format "not supported yet: {}" $ F.Only s


unquote :: S.Expression -> Maybe Text
unquote (S.Atom t) = Just $ dropAround (== '\"') t
unquote _ = Nothing

indented :: LT.Text -> LT.Text
indented t = LT.unlines $ map (LT.append "\t") (LT.lines t)
