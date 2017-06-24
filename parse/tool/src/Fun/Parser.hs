module Fun.Parser (
    parse,
    funImport, funFuncDecl
) where

import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import qualified Fun.Types as Fun

funDef = haskellStyle {
    P.commentStart   = "/*",
    P.commentEnd     = "*/",
    P.commentLine    = "//",
    P.reservedNames = ["package", "func", "case", "if", "then", "else"] -- TODO add more
}

funTokens :: P.TokenParser st
funTokens = P.makeTokenParser funDef

ident :: Parser String
ident = do {
    c <- P.identStart funDef;
    cs <- many (P.identLetter funDef);
    return (c:cs)
    } <?> "identifier"

slash :: Parser Char
slash = char '/'

dot :: Parser String
dot = P.dot funTokens

symbol :: String -> Parser String
symbol = P.symbol funTokens

lexeme :: String -> Parser String
lexeme s = P.lexeme funTokens (string s)

commaSep :: String -> Parser [String]
commaSep s = P.commaSep funTokens (string s)

-- ws :: Parser ()
-- ws = skipMany1 space 

parens :: Parser String -> Parser String
parens = P.parens funTokens

funImport :: Parser Fun.Import
funImport = do
    lexeme "import"
    path <- pathP
    alias <- optionMaybe aliasP
    return $ Fun.Import path alias
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
            lexeme "as"
            P.stringLiteral funTokens

funFuncDecl :: Parser Fun.Decl
funFuncDecl = do
    name <- ident
    spaces
    lexeme "::"
    -- types <- sepBy1 (ident <|> parens commaSep ident) (lexeme " ->")
    types <- sepBy1 ident (lexeme " ->")
    endOfLine
    case reverse types of
        ["IO"]    -> return $ Fun.FuncDecl name [] Fun.JustIO [] Fun.Undefined
        [x]       -> return $ Fun.FuncDecl name [] (Fun.IO $ Fun.Type x) [] Fun.Undefined
        ("IO":xs) -> return $ Fun.FuncDecl name (map Fun.Type (reverse xs)) Fun.JustIO [] Fun.Undefined
        (x:xs)    -> return $ Fun.FuncDecl name (map Fun.Type (reverse xs)) (Fun.IO $ Fun.Type x) [] Fun.Undefined

    -- params <- optionMaybe $ endBy1 ident (lexeme "->")
    -- results <- lexeme "IO" -- count 1 (string "IO")
    -- results <- choice [count 1 (string "IO"), count 1 ident, inBraces (sepBy ident (symbol ","))]
    -- spaces
    -- newline
    -- return $ FuncDecl name types JustIO [] Undefined
    -- return $ FuncDecl name params (toResult results) [] Undefined


-- toResult :: [String] -> Fun.Result
-- toResult ["IO"]  = JustIO
-- toResult [x] = SingleResult x
-- toResult xs  = ResultTuple xs