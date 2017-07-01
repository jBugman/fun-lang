{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Parser where

import Data.List (intercalate)
import Text.Megaparsec ((<|>), optional, try, char, sepBy1, count, runParser, ParseError,
                        manyTill, many, some, anyChar, eof, endBy1, skipMany, skipSome, sepEndBy)
import Text.Megaparsec.String (Parser)

import Fun.Lexer
import qualified Fun.Types as Fun


prs :: Parser a -> String -> Either (ParseError Char _) a
prs rule = runParser rule ""

funImport :: Parser Fun.Import
funImport = do
    rword "import"
    path <- quoted $ joinedBy '/' $ joinedBy '.' $ joinedBy '-' identifier
    alias <- optional (sp *> rword "as" *> quoted identifier)
    return $ Fun.Import path alias
    where
        joinedBy :: Char -> Parser String -> Parser String
        joinedBy c p = intercalate [c] <$> sepBy1 p (char c) -- black Applicative magic

funFuncDecl :: Parser Fun.TopLevel
funFuncDecl = do
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
    return $ map Fun.Atomic xs -- TODO add more

funcParams :: Parser [Fun.Param]
funcParams = do
    xs <- parensList varSpec
    return $ map Fun.Param xs

varSpec :: Parser Fun.VarSpec
varSpec = do
    n <- identifier
    t <- identifier
    return $ Fun.VarSpec n (Fun.Atomic t)

funcBody :: Parser Fun.FuncBody
funcBody = try inline <|> try undef <|> singleExpr

singleExpr :: Parser Fun.FuncBody
singleExpr = do
    e <- expr
    return (Fun.Single e)

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

imports :: Parser [Fun.Import]
imports = sepEndBy funImport (skipSome lf)

package :: Parser Fun.Package
package = do
    name <- rword "package" *> identifier <* skipMany lf
    imps <- imports
    tops <- endBy1 topLevel (skipSome lf <|> eof)
    eof
    return $ Fun.Package name imps tops

topLevel :: Parser Fun.TopLevel
topLevel = funFuncDecl

funcApplication :: Parser Fun.Expr
funcApplication = do
    name <- funcNameP
    args <- try (many expr) <|> return []
    return $ Fun.Application name args

funcNameP :: Parser Fun.FuncName
funcNameP = do
    s <- try selector <|> identifier
    return (Fun.FuncName s)
        where
            selector = do
                rec <- identifier
                _ <- char '.'
                name <- identifier
                return $ rec ++ "." ++ name

expr :: Parser Fun.Expr
expr = try literal <|> funcApplication

literal :: Parser Fun.Expr
literal = do
    lit <- try doubleLit <|> try hexLit <|> try intLit <|> try charLit <|> stringLit
    return (Fun.Lit lit)

doubleLit :: Parser Fun.Literal
doubleLit = do
    x <- double
    return (Fun.DoubleLit x)

hexLit :: Parser Fun.Literal
hexLit = do
    x <- hex
    return (Fun.HexLit x)

intLit :: Parser Fun.Literal
intLit = do
    x <- try signedInteger <|> integer
    return (Fun.IntegerLit x)

stringLit :: Parser Fun.Literal
stringLit = do
    s <- stringLiteral
    return (Fun.StringLit s)

charLit :: Parser Fun.Literal
charLit = do
    c <- charLiteral
    return (Fun.CharLit c)

boolLit :: Parser Fun.Literal
boolLit = do
    s <- try (word "true") <|> word "false"
    return (Fun.BoolLit (s == "true"))
