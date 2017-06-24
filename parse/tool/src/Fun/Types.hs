module Fun.Types where

data Module = Module {
    moduleName :: String,
    imports    :: [Import],
    decls      :: [Decl]
} deriving (Eq, Show)

data Import = Import {
    path  :: String,
    alias :: Maybe String
} deriving (Eq, Show)

data Decl = FuncDecl {
    funcName :: String,
    params   :: [Type],
    result   :: Type,
    args     :: [Var],
    body     :: FuncBody
} deriving (Eq, Show)

type Type = String

type Var = String

data FuncBody = Undefined deriving (Eq, Show)
