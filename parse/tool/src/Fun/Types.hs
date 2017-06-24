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
    params   :: [Param],
    results  :: [Type],
    body     :: FuncBody
} deriving (Eq, Show)

type Var = String

type TypeName = String

data Type = Type TypeName | List Type
    deriving (Eq, Show)

data Param = Param {paramName :: String, paramType :: Type}
    deriving (Eq, Show)

data FuncBody = Undefined
    deriving (Eq, Show)
