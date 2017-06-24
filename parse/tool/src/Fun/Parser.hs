module Fun.Parser where

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (optionMaybe, sepBy1)
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
    -- spaces
    -- params <- sepBy1 identifier (lexeme " ->")
    return $ Fun.FuncDecl name [] [] Fun.Undefined
    -- case reverse types of
    --     ["IO"]    -> return $ Fun.FuncDecl name [] Fun.JustIO [] Fun.Undefined
    --     [x]       -> return $ Fun.FuncDecl name [] (Fun.IO $ Fun.Type x) [] Fun.Undefined
    --     ("IO":xs) -> return $ Fun.FuncDecl name (map Fun.Type (reverse xs)) Fun.JustIO [] Fun.Undefined
    --     (x:xs)    -> return $ Fun.FuncDecl name (map Fun.Type (reverse xs)) (Fun.IO $ Fun.Type x) [] Fun.Undefined

funcParams :: Parser [Fun.Param]
funcParams = do
    xs <- parensList string
    return []