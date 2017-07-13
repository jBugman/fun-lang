{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser where

import ClassyPrelude
import Data.Char               (isDigit, isLower, isUpper)
import Data.Either.Combinators (mapLeft)
import Data.SCargot
import Data.SCargot.Atom
import Data.SCargot.Comments   (withLispComments)
import Data.SCargot.Repr       (SExpr)
import Text.Parsec             (char, choice, satisfy, (<?>))
import Text.Parsec.Char        (string)
import Text.Parsec.Text        (Parser)

import Fun.SExpression (Atom (..), Expression, operators)


parse :: Text -> Either Text Expression
parse = mapLeft pack <$> decodeOne parser

parser :: SExprParser Atom Expression
parser = withLispComments $ asWellFormed parseAtom

parseAtom :: SExprParser Atom (SExpr Atom)
parseAtom = mkAtomParser
    [ atom Op    (pack <$> parseOp)
    , atom Type  (pack <$> parseType)
    , atom Ident (pack <$> parseIdent)
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
