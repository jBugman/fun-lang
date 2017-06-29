module Fun.Types where

type Name = String

data Package = Package Name [Import] [TopLevel]
    deriving (Eq, Show)

data Import = Import String (Maybe Name)
    deriving (Eq, Show)

data TopLevel
    = FuncDecl Name [Param] [Type] FuncBody
        deriving (Eq, Show)

newtype Var = Var String
    deriving (Eq, Show)

data Type
    = Type String
    | Slice Type
    | Map Type Type
        deriving (Eq, Show)

data VarSpec = VarSpec Name Type
    deriving (Eq, Show)

newtype Param = Param VarSpec
    deriving (Eq, Show)

data FuncBody
    = Undefined
    | Single Expr
    | Inline [String]
        deriving (Eq, Show)

newtype FuncName = FuncName String
    deriving (Eq, Show)

data Expr
    = Application FuncName [Expr]
    | Lit Literal
    | DoBlock [Expr]
    | For ForHeader Expr
    | Op Expr Expr
        deriving (Eq, Show)

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
