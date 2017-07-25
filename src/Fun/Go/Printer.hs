{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Printer ( printGo ) where

import ClassyPrelude                hiding (print)
import Data.Either                  (partitionEithers)
import Data.SCargot.Repr.WellFormed (pattern A, pattern L, pattern Nil)
import Data.Text.Buildable          (Buildable)
import Data.Text.Format             (Format, format)
import Data.Text.Format.Params      (Params)

import Fun.Errors        (Error (..))
import Fun.PrettyPrinter (singleLine)
import Fun.SExpression   (Atom (..), Expression, pattern ID, pattern KW, pattern OP, pattern SL,
                          pattern TP)


printGo :: Expression -> Either Error Text
printGo = print

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

print (L [ TP "slice" , x ]) = printf1 "[]{}" <$> print x

print (L [ TP "map" , k@(TP _) , v@(TP _) ])
    = printf2 "map[{}]{}" <$> print k <*> print v

print (L [ TP "func" , a ])
    = printf1 "func({})" <$> printArgs a

print (L [ TP "func" , a , r ])
    = printf2 "func({}) {}" <$> printArgs a <*> printResults r

print (L [ TP "ptr" , x ]) = printf1 "*{}" <$> print x

-- type alias
print (L [ KW "alias" , ID n , e ])
    = printf2 "type {} {}" n <$> print e

-- id-type pair (used in function arguments)
print (L [ ID a , t@(TP _) ])
    = printf2 "{} {}" a <$> print t

print (L [ ID a , t@(L ( TP _ : _)) ])
    = printf2 "{} {}" a <$> print t

-- const
print (L [ KW "const" , ID name , e ])
    = printf2 "const {} = {}" name <$> print e

print (L [ KW "const" , ID name , t@(TP _) , e ])
    = printf3 "const {} {} = {}" name <$> print t <*> print e

-- var
print (L [ KW "var" , ID n , t@(TP _) ])
    = printf2 "var {} {}" n <$> print t

print (L [ KW "var" , x@(ID _) , y@(ID _) , xs ])
    = printf3 "var {}, {} = {}" <$> print x <*> print y <*> print xs

print (L [ KW "var" , ID n , t@(L (TP _ : _)) ])
    = printf2 "var {} {}" n <$> print t

print (L [ KW "var" , ID n , t@(TP _) , e ])
    = printf3 "var {} {} = {}" n <$> print t <*> print e

print (L [ KW "var" , ID n , t@(L (TP "slice" : _)) , L xs ])
    = printf3 "var {} = {}{{}}" n <$> print t <*> printList xs

print (L [ KW "var" , ID n , t@(L (TP "map" : _)) , L xs ])
    = printf3 "var {} = {}{{}}" n <$> print t <*> printList xs

print (L [ KW "var" , ID n , e ]) = printf2 "var {} = {}" n <$> print e

-- struct decl
print (L [ KW "struct" , ID n ])
    = Right $ printf2 "type {} struct{}" n "{}"

print (L ( KW "struct" : ID n : xs ))
    = printf2 "type {} struct {\n{}\n}" n <$> (intercalate "\n" <$> mapM print xs)

-- interface decl
print (L [ KW "interface" , ID n ])
    = Right $ printf2 "type {} interface{}" n "{}"

print (L ( KW "interface" : ID n : xs ))
    = printf2 "type {} interface {\n{}\n}" n
    <$> (intercalate "\n" <$> mapM print' xs)
    where
        print' :: Expression -> Either Error Text
        print' (L [ ID nm , args ])
            = printf2 "{}({})" nm <$> printArgs args

        print' (L [ ID nm , args , res ])
            = printf3 "{}({}) {}" nm <$> printArgs args <*> printResults res

        print' e = mkError "unexpected " e

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

print (L [ KW "func" , ID n , a , body ])
    = printf3 "func {}({}) {\n{}\n}" n <$> printArgs a <*> print body

print (L [ KW "func" , ID n , a , r , body ])
    = printf4 "func {}({}) {} {\n{}\n}" n <$> printArgs a <*> printResults r <*> print body

-- lambda
print (L [ KW "func" , Nil , body ])
    = printf1 "func() {\n{}\n}" <$> print body

print (L [ KW "func" , a , body ])
    = printf2 "func({}) {\n{}\n}" <$> printArgs a <*> print body

print (L [ KW "func" , a , r , body ])
    = printf3 "func({}) {} {\n{}\n}" <$> printArgs a <*> printResults r <*> print body

-- assignment
print (L [ KW "set" , x , xs ])
    = printf2 "{} = {}" <$> print x <*> print xs

print (L [ KW "set" , ID "_", x , xs ])
    = printf2 "_, {} = {}" <$> print x <*> print xs

print (L [ KW "set" , x@(ID _) , y@(ID _) , xs ])
    = printf3 "{}, {} = {}" <$> print x <*> print y <*> print xs

-- indexed access
print (L [ KW "val" , ID n , i ])     = printf2 "{}[{}]" n <$> print i
print (L [ KW "val" , ID n , i , j ]) = printf3 "{}[{}][{}]" n <$> print i <*> print j

-- slicing
print (L [ KW "slice" , ID name , ID "_" , to ])
    = printf2 "{}[:{}]" name <$> print to

print (L [ KW "slice" , ID name , from , ID "_" ])
    = printf2 "{}[{}:]" name <$> print from

print (L [ KW "slice" , ID name , from , to ])
    = printf3 "{}[{}:{}]" name <$> print from <*> print to

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
print (L [ ID f ]) = Right $ printf1 "{}()" f
print (L ( ID f : args )) = printf2 "{}({})" f <$> printList args

-- make
print (L [ KW "make" , s@(L ( TP "slice" : _ )) , d ])
    = printf2 "make({}, {})" <$> print s <*> print d

print (L [ KW "make" , m@(L [ TP "map" , _ , _ ]) ])
    = printf1 "make({})" <$> print m

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
print s = mkError "not supported yet " s



-- Utils --

mkError :: Text -> Expression -> Either Error Text
mkError msg e = Left . TranslationError $ msg <> singleLine e

printFor :: Text -> Expression -> Expression -> Expression -> Expression -> Either Error Text
printFor i from cond iter body = do
    tfrom <- print from
    tcond <- print cond
    titer <- print iter
    tbody <- print body
    Right $ strictFormat "for {} := {}; {}; {} {\n{}\n}" (i, tfrom, tcond, titer, tbody)

printArgs :: Expression -> Either Error Text
printArgs Nil    = Right ""
printArgs (L as) = printList as
printArgs e      = mkError "unexpected" e

printResults :: Expression -> Either Error Text
printResults t@(L ( TP "func" : _ )) = print t
printResults (L as)                  = printf1 "({})" <$> printList as
printResults t@(TP _)                = print t
printResults e                       = mkError "unexpected" e

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
