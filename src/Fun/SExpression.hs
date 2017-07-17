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
    , pattern BL
    , pattern ID
    , pattern TP
    , pattern OP
    , pattern KW
    , operators
) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (WellFormedSExpr (..))
import Data.Text.Buildable          (Buildable, build)
import Data.Text.Lazy.Builder       (fromText)
import GHC.Err                      (errorWithoutStackTrace)
import Numeric                      (showHex)

import Fun.Tokens (keywords, operators)

type Expression = WellFormedSExpr Atom

data Atom
    = Ident   Text
    | Keyword Text
    | Type    Text
    | Op      Text
    | Lit     Literal
    deriving (Eq, Ord, Show)

data Literal
    = Str Text
    | Chr Text
    | Int Integer
    | Hex Integer
    | Dbl Double
    | Bl  Bool
    deriving (Eq, Ord)

pattern ID :: Text -> Expression
pattern ID x = WFSAtom (Ident x)

pattern KW :: Text -> Expression
pattern KW x = WFSAtom (Keyword x)

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

pattern BL :: Bool -> Expression
pattern BL x  = WFSAtom (Lit (Bl x))

instance Ord Expression where
    compare (WFSList a) (WFSList b) = compare a b
    compare (WFSAtom a) (WFSAtom b) = compare a b
    compare (WFSList _) (WFSAtom _) = GT
    compare (WFSAtom _) (WFSList _) = LT

instance IsString Atom where
    fromString s
        | s `elem` operators = Op      (pack s)
        | s `elem` keywords  = Keyword (pack s)
        | ":" `isPrefixOf` s = Type    (pack s)
        | otherwise          = Ident   (pack s)

instance Show Literal where
    show (Str t)    = unpack $ "\"" <> t <> "\""
    show (Chr t)    = unpack $ "'"  <> t <> "'"
    show (Int i)    = show i
    show (Hex h)    = unpack $ "0x" <> showHex h ""
    show (Dbl d)    = show d
    show (Bl True)  = "true"
    show (Bl False) = "false"

instance Buildable Literal where
    build = fromText . tshow

instance Buildable Expression where
    build (WFSAtom x) = case x of
        (Ident s)   -> fromText s
        (Keyword s) -> fromText s
        (Type s)    -> fromText s
        (Op s)      -> fromText s
        (Lit lit)   -> build lit
    build (WFSList x) = errorWithoutStackTrace $ unpack ("Can only print terminal nodes, but got " <> tshow x)
