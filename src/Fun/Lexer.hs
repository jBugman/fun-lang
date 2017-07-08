module Fun.Lexer where

import Control.Applicative       (empty)
import Control.Monad             (void)
import Text.Megaparsec           (alphaNumChar, between, char, char', letterChar, manyTill, oneOf,
                                  some, spaceChar)
import Text.Megaparsec.Text.Lazy (Parser)

import qualified Text.Megaparsec.Lexer as L

import Fun.Sexp (opChars)


lineComment :: Parser ()
lineComment  = L.skipLineComment ";"

sp :: Parser () -- whitespace consumer for lexemes
sp = L.space (void spaceChar) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sp

symbol :: String -> Parser String
symbol = L.symbol sp

word :: String -> Parser String
word = lexeme . symbol

ident :: Parser String
ident = lexeme $ some alphaNumChar

typeLit :: Parser String
typeLit = lexeme $ char ':' *> some alphaNumChar

selector :: Parser String
selector = lexeme $ do
    x    <- letterChar
    xs   <- some alphaNumChar
    void (char '.')
    sel <- some alphaNumChar
    return $ (x : xs) ++ "." ++ sel

op :: Parser Char
op = lexeme $ oneOf opChars

hex :: Parser Integer
hex = lexeme $ char '0' >> char' 'x' >> L.hexadecimal

double :: Parser Double
double = lexeme L.float

integer :: Parser Integer
integer = lexeme L.integer

signedInteger :: Parser Integer
signedInteger = lexeme $ L.signed sp integer

charLiteral :: Parser Char
charLiteral = lexeme $ between tick tick L.charLiteral
    where tick = void $ char '\''

stringLiteral :: Parser String
stringLiteral = lexeme $ do
    xs <- char '"' >> manyTill L.charLiteral (char '"')
    return $ "\"" ++ xs ++ "\""

parens :: Parser a -> Parser a
parens p = lexeme $ between (symbol "(") (symbol ")") p

brackets :: Parser a -> Parser a
brackets p = lexeme $ between (symbol "[") (symbol "]") p

trimWS :: Parser a -> Parser a
trimWS p = sp *> p
