{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Fun.SExpression
    ( Expression
    , Atom (..)
    , Literal (..)
    , pattern SL
    , pattern CL
    , pattern IL
    , pattern HL
    , pattern OL
    , pattern DL
    , pattern BL
    , pattern ID
    , pattern TP
    , pattern OP
    , pattern KW
) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (WellFormedSExpr (..))
import Numeric                      (showHex, showOct)

import qualified Text.PrettyPrint.Leijen.Text as PP


type Expression = WellFormedSExpr Atom

deriving instance Generic Expression

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
    | Oct Integer
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

pattern OL :: Integer -> Expression
pattern OL x = WFSAtom (Lit (Oct x))

pattern DL :: Double -> Expression
pattern DL x = WFSAtom (Lit (Dbl x))

pattern BL :: Bool -> Expression
pattern BL x  = WFSAtom (Lit (Bl x))

instance Ord Expression where
    compare (WFSList a) (WFSList b) = compare a b
    compare (WFSAtom a) (WFSAtom b) = compare a b
    compare (WFSList _) (WFSAtom _) = GT
    compare (WFSAtom _) (WFSList _) = LT

instance PP.Pretty Literal where
    pretty (Str x)    = PP.dquotes . PP.textStrict $ x
    pretty (Chr x)    = PP.squotes . PP.textStrict $ x
    pretty (Int x)    = PP.integer x
    pretty (Hex x)    = PP.text . pack $ "0x" ++ showHex x ""
    pretty (Oct x)    = PP.text . pack $ "0"  ++ showOct x ""
    pretty (Dbl x)    = PP.double x
    pretty (Bl True)  = PP.text "true"
    pretty (Bl False) = PP.text "false"

instance Show Literal where
    show = show . PP.pretty
