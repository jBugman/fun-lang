{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser (parse) where

import ClassyPrelude                  hiding (Int, many, try)
import Data.Char                      (isDigit, isLower, isUpper)
import Data.Either.Combinators        (mapLeft)
import Data.SCargot                   (SExprParser, asWellFormed, decodeOne)
import Data.SCargot.Atom              (atom, mkAtomParser)
import Data.SCargot.Comments          (withLispComments)
import Data.SCargot.Common            (hexNumber, signedDecNumber)
import Data.SCargot.Language.HaskLike (parseHaskellFloat)
import Data.SCargot.Repr              (SExpr)
import Text.Parsec                    (char, choice, many, noneOf, oneOf, satisfy, sepBy1, try,
                                       (<?>))
import Text.Parsec.Char               (string)
import Text.Parsec.Text               (Parser)

import Fun.Errors      (Error (SyntaxError))
import Fun.SExpression (Atom (..), Expression, Literal (..))
import Fun.Tokens      (keywords, operators)


parse :: Text -> Either Error Expression
parse = mapLeft (SyntaxError . pack) <$> decodeOne parser

parser :: SExprParser Atom Expression
parser = withLispComments $ asWellFormed parseAtom

parseAtom :: SExprParser Atom (SExpr Atom)
parseAtom = mkAtomParser
    [ atom Op      (pack <$> parseOp)
    , atom Lit     (Str . pack <$> parseStringLit)
    , atom Lit     (Chr . pack <$> parseCharLit)
    , atom Type    (pack <$> parseType)
    , atom Lit     (Bl   <$> parseBool)
    , identOrKeyword
    , atom Lit     (Dbl  <$> parseFloat)
    , atom Lit     (Hex  <$> parseHexLit)
    , atom Lit     (Int  <$> signedDecNumber)
    ]

identOrKeyword :: Parser Atom
identOrKeyword = do
    ident <- parseIdent
    if ident `elem` keywords
    then return . Keyword . pack $ ident
    else return . Ident   . pack $ ident

parseIdent :: Parser String
parseIdent = string "_" <|> parseSelector

parseSelector :: Parser String
parseSelector = intercalate "." <$> sepBy1 parseIdent' (char '.')

parseIdent' :: Parser String
parseIdent' = (:) <$> (lk <|> uk) <*> many
    (lk <|> uk <|> dg <|> char '_') <?> "ident"
    where
        lk = satisfy isLower
        uk = satisfy isUpper
        dg = satisfy isDigit

parseType :: Parser String
parseType = (:) <$> char ':' *> parseIdent <?> "type literal"

parseOp :: Parser String
parseOp = choice (try . string <$> operators) <?> "operator"

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
parseHexLit = try ((string "0x" <|> string "0X") *> hexNumber) <?> "hex literal"

parseFloat :: Parser Double
parseFloat = try parseHaskellFloat <?> "float"

parseBool :: Parser Bool
parseBool = do
    b <- try (string "false") <|> string "true" <?> "bool"
    return (b == "true")
