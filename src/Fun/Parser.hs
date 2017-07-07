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
sunit = S.Unit <$ (sp *> void (word "()"))

satom :: Parser S.Expression
satom = S.Atom . pack <$> (sp *> atoms)
    where atoms = try stringLiteral <|> try selector <|> ident

sop :: Parser S.Expression
sop = S.Op . singleton <$> (sp *> op)

list :: Parser [S.Expression]
list = sepBy (choice [slist, sunit, stuple, sop, satom]) sp

slist :: Parser S.Expression
slist = S.List <$> (sp *> brackets list)

stuple :: Parser S.Expression
stuple = S.Exp <$> (sp *> parens list)

sexp :: Parser S.Expression
sexp = try stuple <|> try sop <|> satom
