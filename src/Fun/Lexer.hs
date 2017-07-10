module Fun.Lexer where

import           ClassyPrelude
import qualified Text.Megaparsec      as MP
import           Text.Megaparsec.Text (Parser)

import           Fun.Sexp              (opChars)
import qualified Text.Megaparsec.Lexer as L


lineComment :: Parser ()
lineComment  = L.skipLineComment ";"

sp :: Parser () -- whitespace consumer for lexemes
sp = L.space (void MP.spaceChar) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sp

symbol :: Text -> Parser Text
symbol t = pack <$> L.symbol sp (unpack t)

word :: Text -> Parser Text
word = lexeme . symbol

ident :: Parser Text
ident = lexeme $ pack <$> some MP.alphaNumChar

typeLit :: Parser Text
typeLit = lexeme $ pack <$> (MP.char ':' *> some MP.alphaNumChar)

selector :: Parser Text
selector = lexeme $ do
    x    <- MP.letterChar
    xs   <- some MP.alphaNumChar
    void (MP.char '.')
    sel  <- some MP.alphaNumChar
    return . pack . oconcat $ [singleton x, xs, ".", sel]

op :: Parser Text
op = singleton <$> lexeme (MP.oneOf opChars)

hex :: Parser Integer
hex = lexeme $ MP.char '0' >> MP.char' 'x' >> L.hexadecimal

double :: Parser Double
double = lexeme L.float

integer :: Parser Integer
integer = lexeme L.integer

signedInteger :: Parser Integer
signedInteger = lexeme $ L.signed sp integer

charLiteral :: Parser Char
charLiteral = lexeme $ MP.between tick tick L.charLiteral
    where tick = void $ MP.char '\''

stringLiteral :: Parser Text
stringLiteral = lexeme $ do
    xs <- MP.char '"' >> MP.manyTill L.charLiteral (MP.char '"')
    return $ "\"" <> pack xs <> "\""

parens :: Parser a -> Parser a
parens p = lexeme $ MP.between (symbol "(") (symbol ")") p

brackets :: Parser a -> Parser a
brackets p = lexeme $ MP.between (symbol "[") (symbol "]") p

trimWS :: Parser a -> Parser a
trimWS p = sp *> p
