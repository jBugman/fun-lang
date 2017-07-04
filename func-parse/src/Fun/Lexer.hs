module Fun.Lexer where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec (char, char', letterChar, alphaNumChar, spaceChar, between, manyTill, some, symbolChar)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

lineComment :: Parser ()
lineComment  = L.skipLineComment ";"

sp :: Parser () -- whitespace consumer for lexemes
sp = L.space (void spaceChar) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sp

symbol :: String -> Parser String
symbol = L.symbol sp

ident :: Parser String
ident = lexeme $ some alphaNumChar

selector :: Parser String
selector = lexeme $ do
    x    <- letterChar
    xs   <- some alphaNumChar
    void (char '.')
    sel <- some alphaNumChar
    return $ (x : xs) ++ "." ++ sel

op :: Parser String
op = lexeme $ some symbolChar

hex :: Parser Integer
hex = char '0' >> char' 'x' >> L.hexadecimal

double :: Parser Double
double = lexeme L.float

integer :: Parser Integer
integer = lexeme L.integer

signedInteger :: Parser Integer
signedInteger = L.signed sp integer

charLiteral :: Parser Char
charLiteral = between tick tick L.charLiteral
    where tick = void $ char '\''

stringLiteral :: Parser String
stringLiteral = do
    xs <- char '"' >> manyTill L.charLiteral (char '"')
    return $ "\"" ++ xs ++ "\""

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
