{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Printer ( printGo ) where

import ClassyPrelude                hiding (print)
import Data.SCargot.Repr.WellFormed (pattern A, pattern L, pattern Nil)
import Data.Text.Buildable          (Buildable)
import Data.Text.Format             (Format, format)
import Data.Text.Format.Params      (Params)

import Fun.Errors        (Error (..))
import Fun.PrettyPrinter (singleLine)
import Fun.SExpression   (Atom (..), Expression, pattern ID, pattern KW, pattern OP, pattern SL,
                          pattern TP)

type E = Expression

printGo :: E -> Either Error Text
printGo = print

print :: E -> Either Error Text
-- empty
print Nil = Left . TranslationError $ "empty expression"

-- ident
print (ID x) = Right x

-- operator
print (OP x) = Right x

-- literal
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
print (L [ KW "alias" , n@(ID _) , e ])
    = printf2 "type {} {}" <$> print n <*> print e

-- const
print (L [ KW "const" , n@(ID _) , e ])
    = printf2 "const {} = {}" <$> print n <*> print e

print (L [ KW "const" , n@(ID _) , t@(TP _) , e ])
    = printf3 "const {} {} = {}" <$> print n <*> print t <*> print e

-- var
print (L [ KW "var" , n@(ID _) , t@(TP _) ])
    = printf2 "var {} {}" <$> print n <*> print t

print (L [ KW "var" , n@(ID _) , t@(L [TP "slice" , _ ]) ])
    = printf2 "var {} {}" <$> print n <*> print t

print (L [ KW "var" , n@(ID _) , t@(L [TP "map" , _ , _ ]) ])
    = printf2 "var {} {}" <$> print n <*> print t

print (L [ KW "var" , x@(ID _) , y@(ID _) , xs ])
    = printf3 "var {}, {} = {}" <$> print x <*> print y <*> print xs

print (L [ KW "var" , n@(ID _) , t@(TP _) , e ])
    = printf3 "var {} {} = {}" <$> print n <*> print t <*> print e

print (L [ KW "var" , n@(ID _) , L [ t@(L (TP "slice" : _)) , L xs ] ])
    = printf3 "var {} = {}{{}}" <$> print n <*> print t <*> printList xs

print (L [ KW "var" , n@(ID _) , L [ t@(L (TP _ : _)) , L xs ] ])
    = printf3 "var {} = {}{{}}" <$> print n <*> print t <*> printPairs xs

print (L [ KW "var" , n@(ID _) , e ])
    = printf2 "var {} = {}" <$> print n <*> print e

-- struct decl
print (L [ KW "struct" , n@(ID _) ])
    = printf2 "type {} struct{}" <$> print n <*> Right "{}"

print (L ( KW "struct" : n@(ID _) : xs ))
    = printf2 "type {} struct {\n{}\n}"
    <$> print n
    <*> (intercalate "\n" <$> mapM printPair xs)

-- interface decl
print (L [ KW "interface" , n@(ID _) ])
    = printf2 "type {} interface{}" <$> print n <*> Right "{}"

print (L ( KW "interface" : n@(ID _) : xs ))
    = printf2 "type {} interface {\n{}\n}"
    <$> print n
    <*> (intercalate "\n" <$> mapM print' xs)
    where
        print' :: E -> Either Error Text
        print' (L [ nm@(ID _) , args ])
            = printf2 "{}({})" <$> print nm <*> printArgs args

        print' (L [ nm@(ID _) , args , res ])
            = printf3 "{}({}) {}" <$> print nm <*> printArgs args <*> printResults res

        print' e = mkError "invalid interface member: " e

-- struct literal
print (L [ t@(TP _) ])
    = printf2 "{}{}" <$> print t <*> Right "{}"

print (L [ t@(TP _) , L xs ])
    = printf2 "{}{{}}" <$> print t <*> printPairs xs

-- package
print (L ( KW "package" : n@(ID _) : topLevels ))
    = printf2 "package {}\n\n{}"
    <$> print n
    <*> (intercalate "\n\n" <$> mapM print topLevels)

-- import
print (L [ KW "import" , p@(SL _) ])
    = printf1 "import {}" <$> print p

print (L [ KW "import" , p@(SL _) , SL alias ])
    -- Stripping alias of quotes manually
    = printf2 "import {} {}" alias <$> print p

-- func
print (L [ KW "func" , n@(ID _) , b ])
    = printf2 "func {}() {}" <$> print n <*> printBody b

print (L [ KW "func" , n@(ID _) , a , b ])
    = printf3 "func {}({}) {}" <$> print n <*> printArgs a <*> printBody b

print (L [ KW "func" , n@(ID _) , a , r , b ])
    = printf4 "func {}({}) {} {}"
    <$> print n <*> printArgs a <*> printResults r <*> printBody b

-- method
print (L [ KW "method" , o , n@(ID _) , b ])
    = printf3 "func {} {}() {}" <$> printRecv o <*> print n <*> printBody b

print (L [ KW "method" , o , n@(ID _) , a , b ])
    = printf4 "func {} {}({}) {}"
    <$> printRecv o <*> print n <*> printArgs a <*> printBody b

print (L [ KW "method" , o , n@(ID _) , a , r , b ])
    = printf5 "func {} {}({}) {} {}"
    <$> printRecv o <*> print n <*> printArgs a <*> printResults r <*> printBody b

-- lambda
print (L [ KW "func" , Nil , b ])
    = printf1 "func() {}" <$> printBody b

print (L [ KW "func" , a , b ])
    = printf2 "func({}) {}" <$> printArgs a <*> printBody b

print (L [ KW "func" , a , r , b ])
    = printf3 "func({}) {} {}" <$> printArgs a <*> printResults r <*> printBody b

-- assignment
print (L [ KW "set" , x , xs ])
    = printf2 "{} = {}" <$> print x <*> print xs

print (L [ KW "set" , ID "_", x , xs ])
    = printf2 "_, {} = {}" <$> print x <*> print xs

print (L [ KW "set" , x@(ID _) , y@(ID _) , xs ])
    = printf3 "{}, {} = {}" <$> print x <*> print y <*> print xs

-- indexed access
print (L [ KW "val" , n@(ID _) , i ])
    = printf2 "{}[{}]" <$> print n <*> print i

print (L [ KW "val" , n@(ID _) , i , j ])
    = printf3 "{}[{}][{}]" <$> print n <*> print i <*> print j

-- slicing
print (L [ KW "slice" , n@(ID _) , ID "_" , to ])
    = printf2 "{}[:{}]" <$> print n <*> print to

print (L [ KW "slice" , n@(ID _) , from , ID "_" ])
    = printf2 "{}[{}:]" <$> print n <*> print from

print (L [ KW "slice" , n@(ID _) , from , to ])
    = printf3 "{}[{}:{}]" <$> print n <*> print from <*> print to

-- if-then-else
print (L [ KW "if" , cond , thenBr , elseBr@(L ( KW "if" : _ )) ]) -- elseif
    = printf3 "if {} {\n{}\n} else {}"
    <$> print cond <*> print thenBr <*> print elseBr

print (L [ KW "if" , cond , thenBr , elseBr ])
    = printf3 "if {} {\n{}\n} else {\n{}\n}"
    <$> print cond <*> print thenBr <*> print elseBr

print (L [ KW "if" , cond , thenBr ])
    = printf2 "if {} {\n{}\n}" <$> print cond <*> print thenBr

-- for loop
print (L [ KW "for" , body ])
    = printf1 "for {\n{}\n}" <$> print body

print (L [ KW "for" , cond , body ])
    = printf2 "for {} {\n{}\n}" <$> print cond <*> print body

print (L [ KW "for" , i@(ID _) , from , cond , iter , body ])
    = printFor i from cond iter body

print (L [ KW "for" , i@(ID _) , from , to , body ])
    = printFor i from (L [ OP "<" , i , to ]) (L [ OP "++" , i ]) body

-- type conversion
print (L [ KW "cast" , x@(TP _) , y ])
    = printf2 "{}({})" <$> print x <*> print y

print (L [ KW "cast" , x@(L (TP _ : _ )) , y ])
    = printf2 "{}({})" <$> print x <*> print y

-- type conversion
print (L [ KW "assert" , x@(TP _) , y ])
    = printf2 "{}.({})" <$> print y <*> print x

-- function call
print (L [ f@(ID _) ])
    = printf1 "{}()" <$> print f

print (L ( f@(ID _) : args ))
    = printf2 "{}({})" <$> print f <*> printList args

-- make
print (L [ KW "make" , s@(L ( TP "slice" : _ )) , d ])
    = printf2 "make({}, {})" <$> print s <*> print d

print (L [ KW "make" , m@(L [ TP "map" , _ , _ ]) ])
    = printf1 "make({})" <$> print m

-- unary operators
print (L [ OP "++" , x ])   = printf1 "{}++" <$> print x
print (L [ OP "--" , x ])   = printf1 "{}--" <$> print x
print (L [ op@(OP _) , x ]) = printf2 "{}{}" <$> print op <*> print x

-- binary operators
print (L [ OP "=" , lhs , rhs ]) = printf2 "{} == {}" <$> print lhs <*> print rhs

print (L ( op@(OP _) : xs )) = intercalate <$> sep <*> mapM print xs
    where sep = printf1 " {} " <$> print op

-- expression list, recursive
print (L [ L h ] ) = print (L h)
print (L ( L h : rest )) = printf2 "{}\n{}" <$> print (L h) <*> print (L rest)

-- builtins
print (L ( KW "return" : xs ) ) = printf1 "return {}" <$> printList xs

print (L [ KW "range" , x@(ID _) , xs ])
    = printf2 "{} := range {}" <$> print x <*> print xs

print (L [ KW "range" , x@(ID _) , y@(ID _) , xs ])
    = printf3 "{}, {} := range {}" <$> print x <*> print y <*> print xs

-- Keyword-function (continue e.t.c.)
print (L [ KW x ]) = Right x

-- FIXME: catch-all case
print s = mkError "not supported yet " s


-- Sub printers --

printFor :: E -> E -> E -> E -> E -> Either Error Text
printFor i from cond iter body
    = printf5 "for {} := {}; {}; {} {\n{}\n}"
    <$> print i <*> print from <*> print cond <*> print iter <*> print body

printArgs :: E -> Either Error Text
printArgs Nil    = Right ""
printArgs (L as) = intercalate ", " <$> mapM printArg as
    where
    printArg x@(TP _)             = print x
    printArg xs@(L (TP _ : _))    = print xs
    printArg (L [ x@(ID _) , y ]) = printf2 "{} {}" <$> print x <*> print y
    printArg e                    = mkError "invalid arg: " e
printArgs e      = mkError "invalid args: " e

printResults :: E -> Either Error Text
printResults t@(L ( TP "func" : _ )) = print t
printResults (L as)                  = printf1 "({})" <$> printList as
printResults t@(TP _)                = print t
printResults e                       = mkError "invalid results: " e

printPair :: E -> Either Error Text
printPair (L [ x@(ID _) , y ]) = printf2 "{} {}" <$> print x <*> print y
printPair e                    = mkError "invalid pair: " e

printList :: [E] -> Either Error Text
printList xs = intercalate ", " <$> mapM print xs

printPairs :: [E] -> Either Error Text
printPairs xs = intercalate ", " <$> mapM printColonPair xs

printColonPair :: E -> Either Error Text
printColonPair (L [ x@(ID _) , y ])      = printf2 "{}: {}" <$> print x <*> print y
printColonPair (L [ x@(A (Lit _)) , y ]) = printf2 "{}: {}" <$> print x <*> print y
printColonPair e                         = mkError "invalid pair: " e

printBody :: E -> Either Error Text
printBody Nil = Right "{}"
printBody xs  = printf1 "{\n{}\n}" <$> print xs

printRecv :: E -> Either Error Text
printRecv t@(TP _)             = printf1 "({})"    <$> print t
printRecv (L [ n@(ID _) , t ]) = printf2 "({} {})" <$> print n <*> print t
printRecv e                    = mkError "invalid reciever: " e

-- Utils --

mkError :: Text -> E -> Either Error Text
mkError msg e = Left . TranslationError $ msg <> singleLine e

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

printf5 :: Buildable a => Format -> a -> a -> a -> a -> a -> Text
printf5 fmt a b c d e = strictFormat fmt (a, b, c, d, e)
