{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Fun.Parser where

import Data.List (intercalate)
import Text.Megaparsec ((<|>), optional, try, char, sepBy1, count, runParser, ParseError, manyTill, some, anyChar, eof)
import Text.Megaparsec.String (Parser)

import Fun.Lexer
import qualified Fun.Types as Fun


prs :: Parser a -> String -> Either (ParseError Char _) a
prs rule = runParser rule ""

funImport :: Parser Fun.Import
funImport = nonIndented $ do
    rword "import"
    path <- quoted $ joinedBy '/' $ joinedBy '.' $ joinedBy '-' identifier
    alias <- optional (sp *> rword "as" *> quoted identifier)
    return $ Fun.Import path alias
    where
        joinedBy :: Char -> Parser String -> Parser String
        joinedBy c p = intercalate [c] <$> sepBy1 p (char c) -- black Applicative magic

funFuncDecl :: Parser Fun.Decl
funFuncDecl = nonIndented $ do
    rword "func"
    name <- identifier
    params <- try funcParams <|> return []
    results <- try (rword "->" *> funcResults) <|> return []
    rword "="
    body <- funcBody
    return $ Fun.FuncDecl name params results body

funcResults :: Parser [Fun.Type]
funcResults = do
    xs <- try (parensList identifier) <|> try (count 1 identifier) <|> return []
    return $ map Fun.Type xs

funcParams :: Parser [Fun.Param]
funcParams = do
    xs <- parensList (identifier `sepBy1` rword "::")
    return $ map (\[x, y] -> Fun.Param x (Fun.Type y)) xs

funcBody :: Parser Fun.FuncBody
funcBody = inline <|> undef

undef :: Parser Fun.FuncBody
undef = rword "undefined" *> return Fun.Undefined

inline :: Parser Fun.FuncBody
inline = do
    rword "inline"
    lf
    xs <- some indentedLine
    return $ Fun.Inline xs
        where
            indentedLine :: Parser String
            indentedLine = indentation *> manyTill anyChar (lf <|> eof)
