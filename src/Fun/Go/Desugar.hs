module Fun.Go.Desugar (desugar) where

import Data.List        (nub, span)
import Data.Text        (Text)
import Data.Traversable (mapAccumR)

import qualified Fun.Sexp as S


desugar :: S.Expression Text -> S.Expression Text
desugar (S.Exp ("package":name:topLevels)) = S.Exp $ ["package", name] ++ imports ++ decls
    where
        imports = if didSwap then nub $ importFmt : imports' else imports'
        (didSwap, decls)   = swapPrint decls'
        (imports', decls') = span isImport topLevels

desugar e = e


importFmt :: S.Expression Text
importFmt = S.Exp ["import", "\"fmt\""]

isImport :: S.Expression Text -> Bool
isImport (S.Exp ("import":_)) = True
isImport _                    = False


swapPrint :: [S.Expression Text] -> (Bool, [S.Expression Text])
swapPrint = mapAccumR swapPrint' False

swapPrint' :: Bool -> S.Expression Text -> (Bool, S.Expression Text)
swapPrint' _ (S.Atom "print") = (True, S.Atom "fmt.Println")
swapPrint' b (S.List xs)      = S.Exp <$> mapAccumR swapPrint' b xs
swapPrint' b (S.Exp xs)       = S.Exp <$> mapAccumR swapPrint' b xs
swapPrint' b ex               = (b, ex)
