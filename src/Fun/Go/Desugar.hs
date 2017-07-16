{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Desugar (desugar) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (pattern L)
import Data.Traversable             (mapAccumR)

import Fun.SExpression (Expression, pattern ID, pattern KW, pattern SL)


desugar :: Expression -> Expression
desugar (L (KW "package" : ID name : topLevels)) = L $ [ KW "package", ID name ] <> imports <> decls
    where
        imports = if didSwap then ordNub $ importFmt : imports' else imports'
        (didSwap, decls)   = swapPrint decls'
        (imports', decls') = span isImport topLevels

desugar e = e


importFmt :: Expression
importFmt = L [ KW "import" , SL "fmt" ]

isImport :: Expression -> Bool
isImport (L (KW "import" : _)) = True
isImport _                     = False

swapPrint :: [Expression] -> (Bool, [Expression])
swapPrint = mapAccumR go False
    where
        go :: Bool -> Expression -> (Bool, Expression)
        go _ (KW "print") = (True, ID "fmt.Println")
        go b (L xs)       = L <$> mapAccumR go b xs
        go b ex           = (b, ex)
