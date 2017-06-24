module Fun.Lexer where

import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as Tok

reservedNames :: [String]
reservedNames = [
    "import",
    "as",
    "package",
    "func",
    "case",
    "if",
    "then",
    "else"
 ]

reservedOps :: [String]
reservedOps = [
    "+",
    "-"
 ]

nameFirst :: Parser Char
nameFirst = C.alphaNum

nameLetter :: Parser Char
nameLetter = C.alphaNum

lexer = Tok.makeTokenParser Tok.LanguageDef {
    Tok.caseSensitive   = True,
    Tok.commentStart    = "",
    Tok.commentEnd      = "",
    Tok.commentLine     = "//",
    Tok.nestedComments  = False,
    Tok.reservedNames   = reservedNames,
    Tok.reservedOpNames = reservedOps,
    Tok.opStart         = C.oneOf "!",
    Tok.opLetter        = C.oneOf "!",
    Tok.identStart      = nameFirst,
    Tok.identLetter     = nameLetter
}

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

dot :: Parser String
dot = Tok.dot lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

lexeme :: String -> Parser String
lexeme s = Tok.lexeme lexer (C.string s)

comma :: Parser String
comma = Tok.comma lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

parensList :: Parser a -> Parser [a]
parensList p = parens (Tok.commaSep lexer p)

string :: Parser String
string = Tok.stringLiteral lexer
