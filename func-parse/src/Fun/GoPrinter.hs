module Fun.GoPrinter where

import Data.Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as F

import qualified Fun.Sexp as S


newtype SyntaxError = SyntaxError LT.Text deriving (Eq, Show)


print :: S.Expression -> Either SyntaxError LT.Text
-- empty
print (S.Exp []) = Left $ SyntaxError ("empty expression" :: LT.Text)
-- atoms
print (S.Atom s) = Right $ LT.fromStrict s
-- imports
print (S.Exp ["import", path]) = Right $ F.format "import \"{}\"" $ F.Only path
print (S.Exp ["import", path, alias]) = Right $ F.format "import {} \"{}\"" ((unquote alias), path)

print s = Left $ SyntaxError $ F.format "not supported yet: {}" $ F.Only s


unquote :: S.Expression -> Maybe Text
unquote (S.Atom t) = Just $ dropAround (== '\"') t
unquote _ = Nothing
