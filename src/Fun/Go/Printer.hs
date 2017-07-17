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

print (TP x) = Right x

print (L [ TP "slice" , t@(TP _) ]) = printf1 "[]{}" <$> print t

print (L [ TP "map"   , k@(TP _) , v@(TP _) ])
    = printf2 "map[{}]{}" <$> print k <*> print v

-- const
print (L [ KW "const" , ID name , e ])
    = printf2 "const {} = {}" name <$> print e

print (L [ KW "const" , ID name , TP t , e ])
    = printf3 "const {} {} = {}" name t <$> print e

-- var
print (L [ KW "var" , ID n , TP t ])
    = Right $ printf2 "var {} {}" n t

print (L [ KW "var" , ID "_" , ID n , acc@(L ( KW "val" : _)) ])
    = printf2 "var _, {} = {}" n <$> print acc

print (L [ KW "var" , ID n , ct@(L(TP _ : _)) ])
    = printf2 "var {} {}" n <$> print ct

print (L [ KW "var" , ID n , TP t , e ])
    = printf3 "var {} {} = {}" n t <$> print e

print (L [ KW "var" , ID n , ts , L xs ])
    = printf3 "var {} = {}{{}}" n <$> print ts <*> printList xs

print (L [ KW "var" , ID n , e ]) = printf2 "var {} = {}" n <$> print e

-- FIXME: literal pair (used in maps)
print (L [ lit@(A (Lit _)) , ex ]) = printf2 "{}: {}" <$> print lit <*> print ex

-- package
print (L ( KW "package" : ID name : topLevels )) = case partitionEithers (print <$> topLevels) of
    (err : _ , _) -> Left err
    ([] , txts)   -> Right $ printf2 "package {}\n\n{}" name (ointercalate "\n\n" txts)

-- import
print (L [ KW "import" , SL path ])
    = Right $ printf1 "import \"{}\"" path

print (L [ KW "import" , SL path , SL alias ])
    = Right $ printf2 "import {} \"{}\"" alias path

-- func
print (L [ KW "func" , ID n , body ])
    = printf2 "func {}() {\n{}\n}" n <$> print body

print (L [ KW "func" , ID n , L args, body ])
    = printf3 "func {}({}) {\n{}\n}" n <$> printList args <*> print body

print (L [ KW "func" , ID n , L args, TP t, body ])
    = printf4 "func {}({}) {} {\n{}\n}" n <$> printList args <*> print (TP t) <*> print body

print (L [ KW "func" , ID n , L args, L results, body ])
    = printf4 "func {}({}) ({}) {\n{}\n}" n <$> printList args <*> printList results <*> print body

-- assignment
print (L [ KW "set" , ID name , xs ])
    = printf2 "{} = {}" name <$> print xs

print (L [ KW "set" , ID "_", ID name , xs ])
    = printf2 "_, {} = {}" name <$> print xs

print (L [ KW "set" , tar@(L ( KW "val" : _ )) , ex ])
    = printf2 "{} = {}" <$> print tar <*> print ex

-- indexed access
print (L [ KW "val" , ID name , idx ]) = printf2 "{}[{}]" name <$> print idx

-- if-then-else
print (L [ KW "if" , cond , thenBr , elseBr@(L ( KW "if" : _ )) ]) -- elseif
    = printf3 "if {} {\n{}\n} else {}" <$> print cond <*> print thenBr <*> print elseBr

print (L [ KW "if" , cond , thenBr , elseBr ])
    = printf3 "if {} {\n{}\n} else {\n{}\n}" <$> print cond <*> print thenBr <*> print elseBr

print (L [ KW "if" , cond , thenBr ])
    = printf2 "if {} {\n{}\n}" <$> print cond <*> print thenBr

-- for loop
print (L [ KW "for" , body ])
    = printf1 "for {\n{}\n}" <$> print body

print (L [ KW "for" , cond , body ])
    = printf2 "for {} {\n{}\n}" <$> print cond <*> print body

print (L [ KW "for" , ID i , from , cond , iter , body ])
    = printFor i from cond iter body

print (L [ KW "for" , ID i , from , to , body ])
    = printFor i from (L [ OP "<" , ID i , to ]) (L [ OP "++" , ID i]) body

-- function call
print (L ( ID f : args )) = printf2 "{}({})" f <$> printList args

-- unary operators
print (L [ OP "++" , x ]) = printf1 "{}++" <$> print x
print (L [ OP "--" , x ]) = printf1 "{}--" <$> print x
print (L [ OP op , x ])   = printf2 "{}{}" op <$> print x

-- binary operators
print (L [ OP "=" , lhs , rhs ]) = printf2 "{} == {}" <$> print lhs <*> print rhs

print (L (OP op : xs )) = intercalate (" " <> op <> " ") <$> mapM print xs

-- expression list, recursive
print (L [ L h ] ) = print (L h)
print (L ( L h : rest )) = printf2 "{}\n{}" <$> print (L h) <*> print (L rest)

-- builtins
print (L ( KW "return" : xs ) ) = printf1 "return {}" <$> printList xs

print (L [ KW "range" , ID x , xs ])
    = printf2 "{} := range {}" x <$> print xs

print (L [ KW "range" , ID x , ID y, xs ])
    = printf3 "{}, {} := range {}" x y <$> print xs

-- Keyword-function (continue e.t.c.)
print (L [ KW x ]) = Right x

-- FIXME: catch-all case
print s = Left . TranslationError $ "not supported yet " <> singleLine s



-- Utils --

printFor :: Text -> Expression -> Expression -> Expression -> Expression -> Either Error Text
printFor i from cond iter body = do
    tfrom <- print from
    tcond <- print cond
    titer <- print iter
    tbody <- print body
    Right $ strictFormat "for {} := {}; {}; {} {\n{}\n}" (i, tfrom, tcond, titer, tbody)

printList :: [Expression] -> Either Error Text
printList xs = intercalate ", " <$> mapM print xs

strictFormat :: Params ps => Format -> ps -> Text
strictFormat fmt = toStrict . format fmt

printf1 :: Buildable a => Format -> a -> Text
printf1 fmt x = strictFormat fmt [x]

printf2 :: Buildable a => Format -> a -> a -> Text
printf2 fmt x y = strictFormat fmt (x, y)

printf3 :: Buildable a => Format -> a -> a -> a -> Text
printf3 fmt x y z = strictFormat fmt (x, y, z)

printf4 :: Buildable a => Format -> a -> a -> a -> a -> Text
printf4 fmt a b c d = strictFormat fmt (a, b, c, d)
