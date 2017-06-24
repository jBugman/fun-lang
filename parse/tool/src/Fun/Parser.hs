module Fun.Parser where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec ((<|>), try, optionMaybe, sepBy1, count)
import Text.Parsec.Char (char)
import Data.List (intercalate)

import Fun.Lexer
import qualified Fun.Types as Fun

funImport :: Parser Fun.Import
funImport = do
    reserved "import"
    path <- pathP
    alias <- optionMaybe aliasP
    return $ Fun.Import path alias
    where
        pathP = do
            char '"'
            xs <- sepBy1 pp (char '/')
            char '"'
            return $ intercalate "/" xs
                where
                    pp = do
                        xs <- sepBy1 pp' dot
                        return $ intercalate "." xs
                    pp' = do
                        xs <- sepBy1 identifier (char '-')
                        return $ intercalate "-" xs
        aliasP = do
            whitespace
            reserved "as"
            string

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
