module Fun.Go.Desugar (desugar) where

import ClassyPrelude
import Data.Traversable (mapAccumR)

import qualified Fun.SExpression as S


desugar :: S.Expression -> S.Expression
desugar (S.Exp ("package":name:topLevels)) = S.Exp $ ["package", name] <> imports <> decls
    where
        imports = if didSwap then ordNub $ importFmt : imports' else imports'
        (didSwap, decls)   = swapPrint decls'
        (imports', decls') = span isImport topLevels

desugar e = e


importFmt :: S.Expression
importFmt = S.Exp ["import", "\"fmt\""]

isImport :: S.Expression -> Bool
isImport (S.Exp ("import":_)) = True
isImport _                    = False


swapPrint :: [S.Expression] -> (Bool, [S.Expression])
swapPrint = mapAccumR swapPrint' False

swapPrint' :: Bool -> S.Expression -> (Bool, S.Expression)
swapPrint' _ (S.Atom "print") = (True, S.Atom "fmt.Println")
swapPrint' b (S.List xs)      = S.Exp <$> mapAccumR swapPrint' b xs
swapPrint' b (S.Exp xs)       = S.Exp <$> mapAccumR swapPrint' b xs
swapPrint' b ex               = (b, ex)
