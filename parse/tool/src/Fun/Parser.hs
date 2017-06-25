module Fun.Parser where

import Data.List (intercalate)
import Text.Parsec (parse, (<|>), try, ParseError)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (optionMaybe, sepBy1, count)
import Text.Parsec.String (Parser)

import Fun.Lexer
import qualified Fun.Types as Fun

prs :: Parser a -> String -> Either ParseError a
prs rule = parse rule ""

funImport :: Parser Fun.Import
funImport = do
    reserved "import"
    path <- quoted $ joinedBy '/' $ joinedBy '.' $ joinedBy '-' identifier
    alias <- optionMaybe (whitespace *> reserved "as" *> quoted identifier)
    return $ Fun.Import path alias
    where
        joinedBy :: Char -> Parser String -> Parser String
        joinedBy c p = intercalate [c] <$> sepBy1 p (char c) -- black Applicative magic

funFuncDecl :: Parser Fun.Decl
funFuncDecl = do
    reserved "func"
    name <- identifier
    params <- try funcParams <|> return []
    results <- try (reserved "->" *> funcResults) <|> return []
    reservedOp "="
    body <- funcBody
    return $ Fun.FuncDecl name params results body

funcResults :: Parser [Fun.Type]
funcResults = do
    xs <- try (parensList identifier) <|> try (count 1 identifier) <|> return []
    return $ map Fun.Type xs

funcParams :: Parser [Fun.Param]
funcParams = do
    xs <- parensList (identifier `sepBy1` reserved "::")
    return $ map (\[x, y] -> Fun.Param x (Fun.Type y)) xs

funcBody :: Parser Fun.FuncBody
funcBody = do
    reserved "undefined"
    return Fun.Undefined
