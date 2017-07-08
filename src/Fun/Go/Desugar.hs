module Fun.Go.Desugar (desugar) where

import Data.Text (Text)

import qualified Fun.Sexp as S


desugar :: S.Expression Text -> S.Expression Text
desugar e@(S.Exp ("package":_:topLevels)) = id e
--     where
--         imports = filter isImport topLevels

desugar e                                 = id e


isImport :: S.Expression Text -> Bool
isImport (S.Exp ("import":_)) = True
isImport _                    = False
