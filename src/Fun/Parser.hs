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


sunit :: Parser S.Expression
sunit = S.Unit <$ (sp *> void (word "()"))

satom :: Parser S.Expression
satom = S.Atom <$> (sp *> atoms)
    where atoms = try stringLiteral <|> try selector <|> ident

stype :: Parser S.Expression
stype = S.Type <$> (sp *> typeLit)

sop :: Parser S.Expression
sop = S.Op <$> (sp *> op)

list :: Parser [S.Expression]
list = sepBy expr' sp

slist :: Parser S.Expression
slist = S.List <$> (sp *> brackets list)

stuple :: Parser S.Expression
stuple = S.Exp <$> (sp *> parens list)

sexp :: Parser S.Expression
sexp = sp *> expr'

expr' :: Parser S.Expression
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
