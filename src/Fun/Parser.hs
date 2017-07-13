{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser where

import ClassyPrelude
import Data.Char               (isDigit, isLower, isUpper)
import Data.Either.Combinators (mapLeft)
import Data.SCargot
import Data.SCargot.Atom
import Data.SCargot.Comments   (withLispComments)
import Data.SCargot.Repr       (SExpr)
import Text.Parsec             (char, choice, satisfy)
import Text.Parsec.Char        (string)
import Text.Parsec.Text        (Parser)

import qualified Fun.SExpression as S


parse :: Text -> Either Text S.Expression
parse = mapLeft pack <$> decodeOne parser

parser :: SExprParser S.Atom S.Expression
parser = withLispComments $ asWellFormed parseAtom

parseAtom :: SExprParser S.Atom (SExpr S.Atom)
parseAtom = mkAtomParser
    [ atom S.Op    (pack <$> parseOp)
    , atom S.Type  (pack <$> parseType)
    , atom S.Ident (pack <$> parseIdent)
    ]

parseIdent :: Parser String
parseIdent = (:) <$> (lk <|> uk) <*> many
    (lk <|> uk <|> dg <|> char '_')
    where
        lk = satisfy isLower
        uk = satisfy isUpper
        dg = satisfy isDigit

parseType :: Parser String
parseType = (:) <$> char ':' <*> parseIdent

parseOp :: Parser String
parseOp = choice $ fmap string S.operators
