{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser where

import Control.Monad             (void)
import Data.List                 (intercalate)
import Data.List.Split           (splitOn)
import Data.Text                 (Text, pack, singleton)
import Text.Megaparsec           (ParseError, ShowErrorComponent, choice, errorPos, runParser,
                                  sepBy, sourceColumn, sourceLine, try, unPos, (<|>))
import Text.Megaparsec.Error     (parseErrorTextPretty)
import Text.Megaparsec.Text.Lazy (Parser)

import qualified Data.List.NonEmpty as L
import qualified Data.Text.Lazy     as LT

import Fun.Lexer

import qualified Fun.Sexp as S


sunit :: Parser (S.Expression Text)
sunit = S.Unit <$ (sp *> void (word "()"))

satom :: Parser (S.Expression Text)
satom = S.Atom . pack <$> (sp *> atoms)
    where atoms = try stringLiteral <|> try selector <|> ident

stype :: Parser (S.Expression Text)
stype = S.Type . pack <$> (sp *> typeLit)

sop :: Parser (S.Expression Text)
sop = S.Op . singleton <$> (sp *> op)

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

prs :: Parser a -> LT.Text -> Either (ParseError Char _) a
prs rule = runParser rule ""

prettyError :: ShowErrorComponent e => ParseError Char e -> Text
prettyError err = pack $
    msg ++ "at Ln " ++ show line ++ ", Col " ++ show col
        where
            pos  = L.head (errorPos err)
            line = unPos $ sourceLine pos
            col  = unPos $ sourceColumn pos
            msg  = replace "\n" " "
                $ replace "unexpected" "found"
                $ replace "expecting" "expected"
                $ parseErrorTextPretty err

            replace :: Eq a => [a] -> [a] -> [a] -> [a]
            replace old new = intercalate new . splitOn old
