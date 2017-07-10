{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser where

import           ClassyPrelude         hiding (try)
import qualified Data.List.NonEmpty    as L
import           Text.Megaparsec       (ParseError, choice, runParser, sepBy, try, (<|>))
import qualified Text.Megaparsec       as MP
import qualified Text.Megaparsec.Error as MP
import           Text.Megaparsec.Text  (Parser)

import Fun.Lexer

import qualified Fun.Sexp as S


sunit :: Parser (S.Expression Text)
sunit = S.Unit <$ (sp *> void (word "()"))

satom :: Parser (S.Expression Text)
satom = S.Atom <$> (sp *> atoms)
    where atoms = try stringLiteral <|> try selector <|> ident

stype :: Parser (S.Expression Text)
stype = S.Type <$> (sp *> typeLit)

sop :: Parser (S.Expression Text)
sop = S.Op <$> (sp *> op)

list :: Parser [S.Expression Text]
list = sepBy expr' sp

slist :: Parser (S.Expression Text)
slist = S.List <$> (sp *> brackets list)

stuple :: Parser (S.Expression Text)
stuple = S.Exp <$> (sp *> parens list)

sexp :: Parser (S.Expression Text)
sexp = sp *> expr'

expr' :: Parser (S.Expression Text)
expr' = choice [slist, sunit, stuple, stype, sop, satom]


-- Wrapper --

prs :: Parser a -> Text -> Either (ParseError Char _) a
prs rule = runParser rule ""

prettyError :: MP.ShowErrorComponent e => ParseError Char e -> Text
prettyError err = oconcat [msg, "at Ln ", line, ", Col ", col]
    where
        pos  = L.head $ MP.errorPos err
        line = tshow $ MP.unPos $ MP.sourceLine pos
        col  = tshow $ MP.unPos $ MP.sourceColumn pos
        msg  = repl "\n" " "
            $ repl "unexpected" "found"
            $ repl "expecting" "expected"
            $ pack (MP.parseErrorTextPretty err)
        repl = replaceSeqStrictText
