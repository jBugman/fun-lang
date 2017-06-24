module Fun.Parser (
    parse,
    funImport
) where

import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Fun.Types

funTokens :: P.TokenParser st
funTokens = P.makeTokenParser haskellStyle {
    P.reservedNames = ["package", "func", "case", "if", "then", "else"] -- TODO add more
}

ident :: Parser String
ident = P.identifier funTokens

slash :: Parser Char
slash = char '/'

dot :: Parser String
dot = P.dot funTokens

ws :: Parser ()
ws = P.whiteSpace funTokens

funImport :: Parser Import
funImport = do
    ws
    string "import"
    spaces
    path <- pathP
    alias <- optionMaybe aliasP
    return $ Import path alias
    where
        pathP = do
            char '"'
            xs <- sepBy1 pp slash
            char '"'
            return $ intercalate "/" xs
                where
                    pp = do
                        xs <- sepBy1 pp' dot
                        return $ intercalate "." xs
                    pp' = do
                        xs <- sepBy1 ident (char '-')
                        return $ intercalate "-" xs
        aliasP = do
            spaces
            string "as"
            spaces
            P.stringLiteral funTokens
