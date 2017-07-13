{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser (parse) where

import ClassyPrelude           hiding (Int, many)
import Data.Char               (isDigit, isLower, isUpper)
import Data.Either.Combinators (mapLeft)
import Data.SCargot            (SExprParser, asWellFormed, decodeOne)
import Data.SCargot.Atom       (atom, mkAtomParser)
import Data.SCargot.Comments   (withLispComments)
import Data.SCargot.Common     (hexNumber, signedDecNumber)
import Data.SCargot.Repr       (SExpr)
import Text.Parsec             (char, choice, many, noneOf, oneOf, satisfy, (<?>))
import Text.Parsec.Char        (string)
import Text.Parsec.Text        (Parser)

import Fun.SExpression (Atom (..), Expression, Literal (..), operators)


parse :: Text -> Either Text Expression
parse = mapLeft pack <$> decodeOne parser

parser :: SExprParser Atom Expression
parser = withLispComments $ asWellFormed parseAtom

parseAtom :: SExprParser Atom (SExpr Atom)
parseAtom = mkAtomParser
    [ atom Op    (pack <$> parseOp)
    , atom Lit   (Str . pack <$> parseStringLit)
    , atom Lit   (Chr . pack <$> parseCharLit)
    , atom Type  (pack <$> parseType)
    , atom Ident (pack <$> parseIdent)
    , atom Lit   (Hex  <$> parseHexLit)
    , atom Lit   (Int  <$> signedDecNumber)
    ]

parseIdent :: Parser String
parseIdent = (:) <$> (lk <|> uk) <*> many
    (lk <|> uk <|> dg <|> char '_') <?> "ident"
    where
        lk = satisfy isLower
        uk = satisfy isUpper
        dg = satisfy isDigit

parseType :: Parser String
parseType = (:) <$> char ':' *> parseIdent <?> "type literal"

parseOp :: Parser String
parseOp = choice (fmap string operators) <?> "operator"

parseStringLit :: Parser String
parseStringLit = char q *> many (noneOf (singleton q)) <* char q <?> "string literal"
    where q = '"'

parseCharLit :: Parser String
parseCharLit = do
    void $ char '\''
    s <- parseEscape <|> parseSingleChar
    void $ char '\''
    return s
    where
        parseSingleChar = singleton <$> noneOf "\'"
        parseEscape = do
            slash <- char '\\'
            c <- oneOf ['a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\'']
            return [slash, c]

parseHexLit :: Parser Integer
parseHexLit = (string "0x" <|> string "0X") *> hexNumber <?> "hex literal"
