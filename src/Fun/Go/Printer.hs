{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Printer
    ( printPretty
    , print
) where

import ClassyPrelude                hiding (print)
import Data.Either                  (partitionEithers)
import Data.SCargot.Repr.WellFormed (pattern A, pattern L, pattern Nil)
import Data.Text.Buildable          (Buildable)
import Data.Text.Format             (Format, format)
import Data.Text.Format.Params      (Params)

import Fun.Errors      (Error (..))
import Fun.Printer     (singleLine)
import Fun.SExpression (Atom (..), Expression, pattern ID, pattern KW, pattern OP, pattern SL,
                        pattern TP)
import Go.Fmt          (gofmt)


printPretty :: Expression -> Either Error Text
printPretty e = print e >>= gofmt

print :: Expression -> Either Error Text
-- empty
print Nil = Left . TranslationError $ "empty expression"

-- ident
print (ID x) = Right x

-- literal
print (SL x)      = Right $ printf1 "\"{}\"" x
print (A (Lit x)) = Right $ printf1 "{}" x

-- types
print (TP "any") = Right "interface{}"
print (TP x)     = Right x
print (L [ TP "slice" , TP x ])        = Right $ printf1 "[]{}" x
print (L [ TP "map"   , TP k , TP v ]) = Right $ printf2 "map[{}]{}" k v

-- const
print (L [ KW "const" , ID name , e ])        = printf2 "const {} = {}" name <$> print e
print (L [ KW "const" , ID name , TP t , e ]) = printf3 "const {} {} = {}" name t <$> print e

-- var
print (L [ KW "var" , ID name , TP t ])     = Right $ printf2 "var {} {}" name t
print (L [ KW "var" , ID name , e ])        = printf2 "var {} = {}" name <$> print e
print (L [ KW "var" , ID name , TP t , e ]) = printf3 "var {} {} = {}" name t <$> print e
print (L [ KW "var" , ID name , ts , e ])   = do
    te  <- print e
    tts <- print ts
    Right $ printf3 "var {} {} = {}" name tts te

-- package
print (L ( KW "package" : ID name : topLevels )) = case partitionEithers (print <$> topLevels) of
    (err : _ , _) -> Left err
    ([] , txts)   -> Right $ printf2 "package {}\n\n{}" name (ointercalate "\n\n" txts)

-- import
print (L [ KW "import" , SL path ])            = Right $ printf1 "import \"{}\"" path
print (L [ KW "import" , SL path , SL alias ]) = Right $ printf2 "import {} \"{}\"" alias path

-- func
print (L [ KW "func" , ID name , body ]) = printf2 "func {}() {\n{}\n}" name <$> print body

-- assignment
print (L [ KW "set" , ID name , body ]) = printf2 "{} = {}" name <$> print body
print (L [ KW "set" , xs@(L ( KW "val" : _ )) , body ]) = do
    accesor <- print xs
    tbody   <- print body
    Right $ printf2 "{} = {}" accesor tbody

-- indexed access
print (L [ KW "val" , ID name , idx ]) = printf2 "{}[{}]" name <$> print idx

-- function call
print (L ( ID f : args )) = funcCall f args

-- unary operators
print (L [ OP op , x ]) = printf2 "{}{}" op <$> print x

-- binary operators
print (L [ OP op , lhs , rhs ]) = do
    lt <- print lhs
    rt <- print rhs
    let o = if op == "=" then "==" else op
    return $ printf3 "{} {} {}" lt o rt

-- expression list, recursive
print (L [ L h ] ) = print (L h)
print (L ( L h : rest )) = do
    ph    <- print (L h)
    prest <- print (L rest)
    return $ ph <> "\n" <> prest

-- catch-all todo case
print s = Left . TranslationError $ "not supported yet " <> singleLine s


-- Function call printer
funcCall :: Text -> [Expression] -> Either Error Text
funcCall name args = case partitionEithers (print <$> args) of
    (err : _ , _) -> Left err
    ([] , txts)   -> Right $ printf2 "{}({})" name (ointercalate ", " txts)


-- Utils --

strictFormat :: Params ps => Format -> ps -> Text
strictFormat fmt = toStrict . format fmt

printf1 :: Buildable a => Format -> a -> Text
printf1 fmt x = strictFormat fmt [x]

printf2 :: Buildable a => Format -> a -> a -> Text
printf2 fmt x y = strictFormat fmt (x, y)

printf3 :: Buildable a => Format -> a -> a -> a -> Text
printf3 fmt x y z = strictFormat fmt (x, y, z)
