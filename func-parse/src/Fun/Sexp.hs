module Fun.Sexp where

data Expression = Exp [Expression] | List [Expression] | Atom String | Unit
    deriving (Eq, Show)
