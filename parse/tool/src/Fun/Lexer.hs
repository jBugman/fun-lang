module Fun.Lexer where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec (try, many, char, string, letterChar, alphaNumChar, spaceChar, between, notFollowedBy, sepBy, oneOf)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

lineComment :: Parser ()
lineComment  = L.skipLineComment "//"

splf :: Parser () -- whitespace consumer for indentation
splf = L.space (void spaceChar) lineComment empty

sp :: Parser () -- whitespace consumer for lexemes
sp = L.space (void $ oneOf " \t") lineComment empty

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented splf

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sp

symbol :: String -> Parser String
symbol = L.symbol sp

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sp

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

rws :: [String] -- list of reserved words
rws = [
    "import",
    "as",
    "package",
    "func",
    "case",
    "if",
    "then",
    "else"
 ]

integer :: Parser Integer
integer = lexeme L.integer

dot :: Parser String
dot = symbol "."

comma :: Parser String
comma = symbol ","

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` comma

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensList :: Parser a -> Parser [a]
parensList p = parens (commaSep p)

quoted :: Parser a -> Parser a
quoted = between (char '"') (char '"')

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

bracesWhitespace :: Parser a -> Parser a
bracesWhitespace = between (char '{') (char '}')
