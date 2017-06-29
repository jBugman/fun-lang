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

type Var = String

type TypeName = String

data Type = Type TypeName | List Type
    deriving (Eq, Show)

data Param = Param {paramName :: String, paramType :: Type}
    deriving (Eq, Show)

data FuncBody = Undefined | Expr | Inline [String]
    deriving (Eq, Show)

type FuncName = String

data Expr
    = Application {
        func :: FuncName,
        args :: [Expr]
    }
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
