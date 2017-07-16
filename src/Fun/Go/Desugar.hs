{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Desugar (desugar) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (pattern L)
import Data.Traversable             (mapAccumR)

import Fun.SExpression (Expression, pattern ID, pattern KW, pattern SL)


desugar :: Expression -> Expression
desugar (L (KW "package" : ID name : topLevels)) = L $ [ KW "package", ID name ] <> imports <> decls
    where
        (imports, decls) = swapPrint $ span isImport topLevels

desugar e = e


importFmt :: Expression
importFmt = L [ KW "import" , SL "fmt" ]

isImport :: Expression -> Bool
isImport (L (KW "import" : _)) = True
isImport _                     = False

swapPrint :: ([Expression], [Expression]) -> ([Expression], [Expression])
swapPrint (imports, decls) = mapAccumR go imports decls
    where
        go :: [Expression] -> Expression -> ([Expression], Expression)
        go imps (KW "print") = (addFmt imps, ID "fmt.Println")
        go imps (L xs)       = L <$> mapAccumR go imps xs
        go imps ex           = (imps, ex)

        addFmt :: [Expression] -> [Expression]
        addFmt xs = ordNub $ importFmt : xs
