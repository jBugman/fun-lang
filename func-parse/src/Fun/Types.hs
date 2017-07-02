{-# LANGUAGE OverloadedStrings #-}
module Fun.Types where

import Data.Aeson (ToJSON, toJSON, (.=), object)
import Data.Aeson.Types (Pair, Value)

type Name = String

data Package = Package Name [Import] [TopLevel]
    deriving (Eq, Show)

instance ToJSON Package where
    toJSON (Package name imports topLevels) =
        wrap "Package" [ "name" .= name, "imports" .= imports, "topLevels" .= topLevels ]

data Import = Import String (Maybe Name)
    deriving (Eq, Show)

instance ToJSON Import where
    toJSON (Import path Nothing)      = wrap "Import" [ "path" .= path ]
    toJSON (Import path (Just alias)) = wrap "Import" [ "path" .= path, "alias" .= alias ]

data TopLevel
    = FuncDecl Name [Param] [Type] FuncBody
        deriving (Eq, Show)

instance ToJSON TopLevel where
    toJSON (FuncDecl name params results body) = wrap "FuncDecl"
        [ "name"    .= name
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
    toJSON (Atomic v) = wrap "Atomic" [ "v" .= v ]
    toJSON (Slice v)  = wrap "Slice"  [ "v" .= v ]
    toJSON (Map k v)  = wrap "Map"    [ "k" .= k, "v" .= v ]

data Field = Field Name Type
    deriving (Eq, Show)

instance ToJSON Field where
    toJSON (Field n t) = wrap "Field" [ "name" .= n, "type" .= t ]

newtype Param = Param Field
    deriving (Eq, Show)

instance ToJSON Param where
    toJSON (Param v) = wrap "Param" [ "v" .= v ]

data FuncBody
    = Undefined
    | Single Expr
    | Inline [String]
        deriving (Eq, Show)

instance ToJSON FuncBody where
    toJSON Undefined      = wrap "Undefined" []
    toJSON (Single expr)  = wrap "Single"    [ "expr" .= expr ]
    toJSON (Inline block) = wrap "Inline"    [ "block" .= block ]

newtype FuncName = FuncName String
    deriving (Eq, Show)

instance ToJSON FuncName where
    toJSON (FuncName s) = wrap "FuncName" [ "v" .= s ]

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
    toJSON (Application name args) = wrap "Application" [ "name" .= name, "args" .= args ]
    toJSON (Lit lit)               = toJSON lit -- see 'instance ToJSON Literal'
    toJSON (DoBlock exprs)         = wrap "DoBlock" [ "exprs" .= exprs ]
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
    toJSON (StringLit x)  = wrap "StringLit"  [ "v" .= x ]
    toJSON (CharLit x)    = wrap "CharLit"    [ "v" .= x ]
    toJSON (IntegerLit x) = wrap "IntegerLit" [ "v" .= x ]
    toJSON (DoubleLit x)  = wrap "DoubleLit"  [ "v" .= x ]
    toJSON (HexLit x)     = wrap "HexLit"     [ "v" .= x ]
    toJSON (BoolLit x)    = wrap "BoolLit"    [ "v" .= x ]

wrap :: String -> [Pair] -> Value
wrap t ps = object [ jsonType t, "$data" .= object ps ]
    where
        jsonType :: String -> Pair
        jsonType s = "$type" .= s
