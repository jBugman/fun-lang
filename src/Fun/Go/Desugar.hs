{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Desugar (desugar) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (pattern L)
import Data.Traversable             (mapAccumR)

import Fun.SExpression (Expression, pattern ID, pattern KW, pattern SL)

type E = Expression

desugar :: E -> E
desugar (L (KW "package" : ID name : topLevels))
    = L $ [ KW "package", ID name ] <> imports <> decls
    where
        (imports, decls) = swapPrintf . swapPrint . span isImport $ topLevels

        swapPrint  = swapAddImport importFmt (== KW "print")  (ID "fmt.Println")
        swapPrintf = swapAddImport importFmt (== KW "printf") (ID "fmt.Printf")

-- ignore non-package
desugar e = e


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
            | cond ex    = (addFmt imps, to)
            | L xs <- ex = L <$> mapAccumR go imps xs
            | otherwise  = (imps, ex)

        addFmt :: [E] -> [E]
        addFmt xs = ordNub $ imp : xs
