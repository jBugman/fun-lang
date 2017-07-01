{-# LANGUAGE OverloadedStrings #-}
module Fun.Types where

import Data.Aeson (ToJSON, toJSON, (.=), object)
import Data.Aeson.Types (Pair)

type Name = String

data Package = Package Name [Import] [TopLevel]
    deriving (Eq, Show)

instance ToJSON Package where
    toJSON (Package name imports topLevels) =
        object [ jsonType "Package", "name" .= name, "imports" .= imports, "topLevels" .= topLevels ]

data Import = Import String (Maybe Name)
    deriving (Eq, Show)

instance ToJSON Import where
    toJSON (Import path Nothing)      = object [ jsonType "Import", "path" .= path ]
    toJSON (Import path (Just alias)) = object [ jsonType "Import", "path" .= path, "alias" .= alias ]

data TopLevel
    = FuncDecl Name [Param] [Type] FuncBody
        deriving (Eq, Show)

instance ToJSON TopLevel where
    toJSON (FuncDecl name params results body) = object
        [ jsonType "FuncDecl"
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
    toJSON (Atomic t) = object [ jsonType "Atomic", "value" .= t ]
    toJSON (Slice t)  = object [ jsonType "Slice", "type" .= t ]
    toJSON (Map k v)  = object [ jsonType "Map", "keys" .= k, "values" .= v ]

data VarSpec = VarSpec Name Type
    deriving (Eq, Show)

instance ToJSON VarSpec where
    toJSON (VarSpec n t) = object [ jsonType "VarSpec", "name" .= n, "type" .= t ]

newtype Param = Param VarSpec
    deriving (Eq, Show)

instance ToJSON Param where
    toJSON (Param v) = object [ jsonType "Param", "v" .= v ]

data FuncBody
    = Undefined
    | Single Expr
    | Inline [String]
        deriving (Eq, Show)

instance ToJSON FuncBody where
    toJSON Undefined      = object [ jsonType "Undefined" ]
    toJSON (Single expr)  = object [ jsonType "Single", "expr" .= expr ]
    toJSON (Inline block) = object [ jsonType "Inline", "block" .= block ]

newtype FuncName = FuncName String
    deriving (Eq, Show)

instance ToJSON FuncName where
    toJSON (FuncName s) = object [ jsonType "FuncName", "v" .= s ]

data Expr
    = Application FuncName [Expr]
    | Lit Literal
    | DoBlock [Expr]
    -- TODO Var is really an Expr
    -- TODO add return list
    -- | For ForHeader Expr
    -- | BinaryOp Op Expr Expr
        deriving (Eq, Show)

instance ToJSON Expr where
    toJSON (Application name args) = object [ jsonType "Application", "name" .= name, "args" .= args ]
    toJSON (Lit lit)               = toJSON lit
    toJSON (DoBlock exprs)         = object [ jsonType "DoBlock", "exprs" .= exprs ]
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
    | CharLit Char
    | IntegerLit Integer
    | DoubleLit Double
    | HexLit Integer
    | BoolLit Bool
        deriving (Eq, Show)

instance ToJSON Literal where
    toJSON (StringLit x)  = object [ jsonType "StringLit", "value" .= x ]
    toJSON (CharLit x)    = object [ jsonType "CharLit", "value" .= x ]
    toJSON (IntegerLit x) = object [ jsonType "IntegerLit", "value" .= x ]
    toJSON (DoubleLit x)  = object [ jsonType "DoubleLit", "value" .= x ]
    toJSON (HexLit x)     = object [ jsonType "HexLit", "value" .= x ]
    toJSON (BoolLit x)    = object [ jsonType "BoolLit", "value" .= x ]

jsonType :: String -> Pair
jsonType s = "$type" .= s
