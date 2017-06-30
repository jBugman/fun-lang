{-# LANGUAGE OverloadedStrings #-}
module Fun.Types where

import Data.Aeson (ToJSON, toJSON, (.=), object)
import Data.Aeson.Types (Pair)

type Name = String

data Package = Package Name [Import] [TopLevel]
    deriving (Eq, Show)

instance ToJSON Package where
    toJSON (Package name imports topDecls) =
        object [ jsonType "package", "name" .= name, "imports" .= imports, "topDecls" .= topDecls ]

data Import = Import String (Maybe Name)
    deriving (Eq, Show)

instance ToJSON Import where
    toJSON (Import path Nothing)      = object [ jsonType "import", "path" .= path ]
    toJSON (Import path (Just alias)) = object [ jsonType "import", "path" .= path, "alias" .= alias ]

data TopLevel
    = FuncDecl Name [Param] [Type] FuncBody
        deriving (Eq, Show)

instance ToJSON TopLevel where
    toJSON (FuncDecl name params results body) = object
        [ jsonType "funcDecl"
        , "name"    .= name
        , "params"  .= params
        , "results" .= results
        , "body"    .= body
        ]

newtype Var = Var String
    deriving (Eq, Show)

data Type
    = Atomic String
    | Slice Type
    | Map Type Type
        deriving (Eq, Show)

instance ToJSON Type where
    toJSON (Atomic t) = object [ jsonType "atomic", "value" .= t ]
    toJSON (Slice t)  = object [ jsonType "sliceOf", "type" .= t ]
    toJSON (Map k v)  = object [ jsonType "mapOf", "keys" .= k, "values" .= v ]

data VarSpec = VarSpec Name Type
    deriving (Eq, Show)

instance ToJSON VarSpec where
    toJSON (VarSpec n t) = object [ jsonType "varSpec", "name" .= n, "type" .= t ]

newtype Param = Param VarSpec
    deriving (Eq, Show)

instance ToJSON Param where
    toJSON (Param v) = object [ jsonType "param", "value" .= v ]

data FuncBody
    = Undefined
    | Single Expr
    | Inline [String]
        deriving (Eq, Show)

instance ToJSON FuncBody where
    toJSON Undefined      = "undefined"
    toJSON (Single expr)  = object [ jsonType "singleExpr", "expr" .= expr ]
    toJSON (Inline block) = object [ jsonType "inline", "block" .= block ]

newtype FuncName = FuncName String
    deriving (Eq, Show)

instance ToJSON FuncName where
    toJSON (FuncName s) = toJSON s

data Expr
    = Application FuncName [Expr]
    | Lit Literal
    | DoBlock [Expr]
    -- | For ForHeader Expr
    -- | Op Expr Expr
        deriving (Eq, Show)

instance ToJSON Expr where
    toJSON (Application name args) = object [ jsonType "funcApplication", "name" .= name, "args" .= args ]
    toJSON (Lit lit)               = toJSON lit
    toJSON (DoBlock expressions)   = object [ jsonType "doBlock", "expressions" .= expressions ]
    -- toJSON For ForHeader Expr
    -- toJSON Op Expr Expr

data ForHeader
    = Ever
    | Iter Var Int Int
    | IterInlusive Var Int Int
    | Range1 Var Type
    | Range2 Var Var Type
        deriving (Eq, Show)

data Literal
    = StringLit String
    | IntegerLit Integer
    | DoubleLit Double
    | HexLit Integer
    | BoolLit Bool
        deriving (Eq, Show)

instance ToJSON Literal where
    toJSON (StringLit x)  = object [ jsonType "stringLit", "value" .= x ]
    toJSON (IntegerLit x) = object [ jsonType "integerLit", "value" .= x ]
    toJSON (DoubleLit x)  = object [ jsonType "doubleLit", "value" .= x ]
    toJSON (HexLit x)     = object [ jsonType "hexLit", "value" .= x ]
    toJSON (BoolLit x)    = object [ jsonType "boolLit", "value" .= x ]

jsonType :: String -> Pair
jsonType s = "$type" .= s
