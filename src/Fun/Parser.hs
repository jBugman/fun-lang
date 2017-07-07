{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser where

import Control.Monad        (void)
import Data.Text            (Text, pack, singleton)
import Text.Megaparsec      (ParseError, choice, runParser, sepBy, try, (<|>))
import Text.Megaparsec.Text (Parser)

import Fun.Lexer

import qualified Fun.Sexp as S


prs :: Parser a -> Text -> Either (ParseError Char _) a
prs rule = runParser rule ""


sunit :: Parser S.Expression
sunit = trimWS $ do
    void $ word "()"
    return S.Unit

satom :: Parser S.Expression
satom = trimWS $ do
    s <- try stringLiteral <|> try selector <|> ident
    return $ S.Atom (pack s)

sop :: Parser S.Expression
sop = trimWS $ do
    s <- op
    return $ S.Op (singleton s)

list :: Parser [S.Expression]
list = sepBy (choice [slist, sunit, stuple, sop, satom]) sp

slist :: Parser S.Expression
slist = trimWS $ do
    xs <- brackets list
    return (S.List xs)

stuple :: Parser S.Expression
stuple = trimWS $ do
    xs <- parens list
    return (S.Exp xs)

sexp :: Parser S.Expression
sexp = try stuple <|> try sop <|> satom
