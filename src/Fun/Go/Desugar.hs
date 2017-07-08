module Fun.Go.Desugar where

import qualified Fun.Sexp as S

desugar :: S.Expression -> S.Expression
desugar = id
