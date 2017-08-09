{-# LANGUAGE PatternSynonyms #-}
module Fun.Desugar (desugar) where

import ClassyPrelude
import Data.Traversable (mapAccumR)
import GHC.Err          (errorWithoutStackTrace)

import Fun.SExpression (pattern A, Expression (..), pattern ID, pattern KW, pattern L, pattern LIT,
                        Literal (..), pattern OP)

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
importFmt = L [ KW "import" , LIT (String "fmt") Nothing ]
-- TODO: Probably a problem with inserting expressions without position.


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
addReturn (L [ KW "method" , recv , name , args , results , body ])
    = L [ KW "method" , recv , name , args , results , withReturn body ]
addReturn (L xs)                                             = L (addReturn <$> xs)
addReturn ex                                                 = ex

withReturn :: E -> E
withReturn (A a)                      = L [KW "return" , A a ]
withReturn ret@(L (KW "return" : _ )) = ret
withReturn b
    | isExpression b = L [KW "return" , b ]
    | L exprs <- b   = case unsnoc exprs of
        Just (xs, x) -> L $ xs `snoc` withReturn x
    -- Throwing exceptions here for to catch edge cases.
    -- These conditions should not be met.
        Nothing      -> errorWithoutStackTrace ("addReturn: " <> show b)
    | otherwise      =  errorWithoutStackTrace ("addReturn: " <> show b)


isExpression :: E -> Bool
isExpression (Atom _ _)                 = True
isExpression (List ( OP _ _ : _ ) _)    = True
isExpression (List ( ID _ : _ ) _)      = True
isExpression (List ( KW "val"  : _ ) _) = True
isExpression (List ( KW "func" : _ ) _) = True
isExpression _                          = False
