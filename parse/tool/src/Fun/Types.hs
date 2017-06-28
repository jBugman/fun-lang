module Fun.Types where

data Package = Package {
    packageName :: String,
    imports     :: [Import],
    decls       :: [Decl]
} deriving (Eq, Show)

data Import = Import {
    path  :: String,
    alias :: Maybe String
} deriving (Eq, Show)

data Decl
    = FuncDecl {
        funcName :: String,
        params   :: [Param],
        results  :: [Type],
        body     :: FuncBody
    }
        deriving (Eq, Show)

newtype Var = Var String deriving (Eq, Show)

data Type = Type String | List Type
    deriving (Eq, Show)

data Param = Param {paramName :: String, paramType :: Type}
    deriving (Eq, Show)

data FuncBody = Undefined | Single Expr | Inline [String]
    deriving (Eq, Show)

newtype FuncName = FuncName String deriving (Eq, Show)

data Expr
    = Application {
        func :: FuncName,
        args :: [Expr]
    }
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
        deriving (Eq, Show)
