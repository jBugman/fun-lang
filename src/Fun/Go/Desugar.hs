{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Desugar (desugar) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (pattern A, pattern L)
import Data.Traversable             (mapAccumR)
import GHC.Err                      (errorWithoutStackTrace)

import Fun.SExpression (Expression, pattern ID, pattern KW, pattern OP, pattern SL)

type E = Expression

desugar :: E -> E
desugar (L (KW "package" : ID name : topLevels))
    = L $ [ KW "package", ID name ] <> imports <> decls
    where
        (imports, decls) = swapWith addReturn
                         . swapPrintf
                         . swapPrint
                         . span isImport $ topLevels

        swapPrint  = swapAddImport importFmt (== KW "print")  (ID "fmt.Println")
        swapPrintf = swapAddImport importFmt (== KW "printf") (ID "fmt.Printf")

desugar e = e -- Ignore non-package


importFmt :: E
importFmt = L [ KW "import" , SL "fmt" ]


isImport :: E -> Bool
isImport (L (KW "import" : _)) = True
isImport _                     = False


swapAddImport :: E -> (E -> Bool) -> E -> ([E], [E]) -> ([E], [E])
swapAddImport imp cond to (imports, decls) = mapAccumR go imports decls
    where
        go :: [E] -> E -> ([E], E)
        go imps ex
            | cond ex    = (addImport imps, to)
            | L xs <- ex = L <$> mapAccumR go imps xs
            | otherwise  = (imps, ex)

        addImport :: [E] -> [E]
        addImport xs = ordNub $ imp : xs

swapWith :: (E -> E) -> ([E], [E]) -> ([E], [E])
swapWith f (imports, decls) = (imports, fmap f decls)

addReturn :: E -> E
addReturn (L [ KW "func" , name , args , results , body ])
    = L [ KW "func" , name , args , results , withReturn body ]
    where
        withReturn (A a)                      = L [KW "return" , A a ]
        withReturn ret@(L (KW "return" : _ )) = ret
        withReturn b
            | isExpression b = L [KW "return" , b ]
            | L exprs <- b   = case unsnoc exprs of
                Nothing      -> b  -- This is really a syntax error, body cannot be empty here
                Just (xs, x) -> L $ xs `snoc` withReturn x
            -- FIXME: throwing for now to catch edge cases
            | otherwise      = errorWithoutStackTrace ("addReturn: " <> show b)

addReturn (L xs)                                             = L $ fmap addReturn xs
addReturn ex                                                 = ex


isExpression :: E -> Bool
isExpression (A _)                 = True
isExpression (L ( OP _ : _ ))      = True
isExpression (L ( ID _ : _ ))      = True
isExpression (L ( KW "val"  : _ )) = True
isExpression (L ( KW "spec" : _ )) = True
isExpression (L ( KW "func" : _ )) = True
isExpression _                     = False
