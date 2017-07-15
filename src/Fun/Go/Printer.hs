{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Printer
    ( SyntaxError(..)
    , unError
    , printPretty
    , print
) where

import ClassyPrelude                hiding (print)
import Data.Either                  (partitionEithers)
import Data.Either.Combinators      (mapLeft)
import Data.SCargot.Repr.WellFormed (pattern A, pattern L, pattern Nil)
import Data.Text.Buildable          (Buildable)
import Data.Text.Format             (Format, format)
import Data.Text.Format.Params      (Params)

import Fun.Printer     (singleLine)
import Fun.SExpression (Atom (..), Expression, pattern ID, pattern OP, pattern SL)
import Go.Fmt          (gofmt)


printPretty :: Expression -> Either SyntaxError Text
printPretty e = print e >>= (mapLeft SyntaxError . gofmt)

print :: Expression -> Either SyntaxError Text
-- empty
print Nil = syntaxErr "empty expression"

-- ident
print (ID x) = Right x

-- literal
print (SL x)      = Right $ printf1 "\"{}\"" x
print (A (Lit x)) = Right $ printf1 "{}"     x

-- package
print (L ( ID "package" : ID name : topLevels )) = case partitionEithers $ fmap print topLevels of
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

-- operators
print (L [ OP op , lhs , rhs]) = do
    lt <- print lhs
    rt <- print rhs
    let o = if op == "=" then "==" else op
    return $ strictFormat "{} {} {}" (lt, o, rt)

-- catch-all todo case
print s = syntaxErr $ "not supported yet: " <> singleLine s


-- Function call printer
funcCall :: Text -> [Expression] -> Either SyntaxError Text
funcCall name args = case partitionEithers $ fmap print args of
    (err : _ , _) -> Left err
    ([] , txts)   -> Right $ printf2 "{}({})" name (ointercalate ", " txts)


-- Types --

newtype SyntaxError = SyntaxError Text deriving (Eq, Show)

unError :: SyntaxError -> String
unError (SyntaxError err) = unpack err

-- Utils --

strictFormat :: Params ps => Format -> ps -> Text
strictFormat fmt = toStrict . format fmt

printf1 :: Buildable a => Format -> a -> Text
printf1 fmt x = strictFormat fmt [x]

printf2 :: Buildable a => Format -> a -> a -> Text
printf2 fmt x y = strictFormat fmt (x, y)

syntaxErr :: Text -> Either SyntaxError Text
syntaxErr = Left . SyntaxError
