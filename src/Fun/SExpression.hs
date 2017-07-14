{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
module Fun.SExpression
    ( Expression
    , Atom (..)
    , Literal (..)
    , pattern SL
    , pattern CL
    , pattern IL
    , pattern HL
    , pattern DL
    , pattern ID
    , pattern TP
    , pattern OP
    , operators
) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (WellFormedSExpr (..))
import Data.Text.Buildable          (Buildable, build)
import Data.Text.Lazy.Builder       (fromText)
import GHC.Err                      (errorWithoutStackTrace)

type Expression = WellFormedSExpr Atom

data Atom
    = Ident Text
    | Type  Text
    | Op    Text
    | Lit   Literal
    deriving (Eq, Ord, Show)

data Literal
    = Str Text
    | Chr Text
    | Int Integer
    | Hex Integer
    | Dbl Double
    deriving (Eq, Ord, Show)

pattern ID :: Text -> Expression
pattern ID x = WFSAtom (Ident x)

pattern TP :: Text -> Expression
pattern TP x = WFSAtom (Type x)

pattern OP :: Text -> Expression
pattern OP x = WFSAtom (Op x)

pattern SL :: Text -> Expression
pattern SL x = WFSAtom (Lit (Str x))

pattern CL :: Text -> Expression
pattern CL x = WFSAtom (Lit (Chr x))

pattern IL :: Integer -> Expression
pattern IL x = WFSAtom (Lit (Int x))

pattern HL :: Integer -> Expression
pattern HL x = WFSAtom (Lit (Hex x))

pattern DL :: Double -> Expression
pattern DL x = WFSAtom (Lit (Dbl x))

instance Ord Expression where
    compare (WFSList a) (WFSList b) = compare a b
    compare (WFSAtom a) (WFSAtom b) = compare a b
    compare (WFSList _) (WFSAtom _) = GT
    compare (WFSAtom _) (WFSList _) = LT

instance IsString Atom where
    fromString s
        | s `elem` operators = Op    (pack s)
        | ":" `isPrefixOf` s = Type  (pack s)
        | otherwise          = Ident (pack s)

instance Buildable Literal where
    build (Str t) = fromText t
    build (Chr t) = fromText t
    build (Int i) = fromText $ tshow i
    build (Hex i) = fromText $ tshow i  -- TODO: proper printing
    build (Dbl d) = fromText $ tshow d

instance Buildable Expression where
    build (WFSAtom x) = case x of
        (Ident s) -> fromText s
        (Type s)  -> fromText s
        (Op s)    -> fromText s
        (Lit lit) -> build lit
    build (WFSList x) = errorWithoutStackTrace $ unpack ("Can only print terminal nodes, but got " <> tshow x)

-- TODO: use s-cargot printer
-- instance Show Expression where
--     show (List xs) = unpack $ mconcat ["(", showContents xs, ")"]
--     show (Ident s) = unpack s
--     show (Type s)  = unpack s
--     show (Op s)    = unpack s

-- showContents :: [Expression] -> Text
-- showContents xs = ointercalate " " $ fmap tshow xs -- TODO: add line-fold on long lists and some keywords

operators :: [String]
operators =
    [ "="
    , "+"
    , "-"
    , "*"
    , "/"
    , "<"
    , ">"
    , "%"
    , "&"
    , "&&"
    , "||"
    ]
