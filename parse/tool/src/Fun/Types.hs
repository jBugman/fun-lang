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

data Decl = FuncDecl {
    funcName :: String,
    params   :: [Type],
    result   :: Result,
    args     :: [Var],
    body     :: FuncBody
} deriving (Eq, Show)

type Var = String

type TypeName = String

data Type = Int | Type TypeName | Tuple [TypeName]
    deriving (Eq, Show)

data Result = JustIO | IO Type | Pure Type
    deriving (Eq, Show)

data FuncBody = Undefined deriving (Eq, Show)
