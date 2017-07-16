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
import Fun.SExpression (Atom (..), Expression, pattern ID, pattern OP, pattern SL, pattern TP)
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
print (TP x) = Right x

-- package
print (L ( ID "package" : ID name : topLevels )) = case partitionEithers (print <$> topLevels) of
    (err : _ , _) -> Left err
    ([] , txts)   -> Right $ printf2 "package {}\n\n{}" name (ointercalate "\n\n" txts)

-- import
print (L [ ID "import" , SL path ])            = Right $ printf1 "import \"{}\"" path
print (L [ ID "import" , SL path , SL alias ]) = Right $ printf2 "import {} \"{}\"" alias path

-- func
print (L [ ID "func" , ID name , body ]) = printf2 "func {}() {\n{}\n}" name <$> print body

-- assignment
print (L [ ID "set" , ID name , body ]) = printf2 "{} = {}" name <$> print body

-- function call
print (L ( ID f : args )) = funcCall f args

-- unary operators
print (L [ OP op , x ]) = printf2 "{}{}" op <$> print x

-- binary operators
print (L [ OP op , lhs , rhs ]) = do
    lt <- print lhs
    rt <- print rhs
    let o = if op == "=" then "==" else op
    return $ strictFormat "{} {} {}" (lt, o, rt)

-- expression list, recursive
print (L [ L h ] ) = print (L h)
print (L ( L h : rest )) = do
    ph    <- print (L h)
    prest <- print (L rest)
    return $ ph <> "\n" <> prest

-- catch-all todo case
print s = Left . TranslationError $ "not supported yet: " <> singleLine s


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
