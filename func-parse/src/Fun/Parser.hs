{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser where

import Data.Text (Text, pack)
import Text.Megaparsec (ParseError, runParser, (<|>), try, choice, sepBy)
import Text.Megaparsec.Text (Parser)

import Fun.Lexer
import qualified Fun.Sexp as S


prs :: Parser a -> Text -> Either (ParseError Char _) a
prs rule = runParser rule ""


sunit :: Parser S.Expression
sunit = do
    _ <- symbol "()"
    return S.Unit

satom :: Parser S.Expression
satom = do
    s <- try stringLiteral <|> try op <|> try selector <|> ident
    return $ S.Atom (pack s)

list :: Parser [S.Expression]
list = do 
    xs <- sepBy (choice [slist, sunit, stuple, satom]) sp
    return xs

slist :: Parser S.Expression
slist = do
    xs <- brackets list
    return (S.List xs)

stuple :: Parser S.Expression
stuple = do
    xs <- parens list
    return (S.Exp xs)

sexp :: Parser S.Expression
sexp = do
    s <- try stuple <|> satom
    return s
