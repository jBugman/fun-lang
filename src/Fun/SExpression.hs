{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Fun.SExpression where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed
import Data.Text.Buildable
import Data.Text.Lazy.Builder       (fromText)
import GHC.Err                      (errorWithoutStackTrace)

type Expression = WellFormedSExpr Atom

data Atom
    = Ident Text
    | Type  Text
    | Op    Text
    | Lit   Lit
    deriving (Eq, Ord, Show)

data Lit
    = S Text
    | I Integer
    | D Double
    deriving (Eq, Ord, Show)

instance IsString Atom where
    fromString s
        | s `elem` operators = Op    (pack s)
        | ":" `isPrefixOf` s = Type  (pack s)
        | otherwise          = Ident (pack s)

instance Buildable Lit where
    build (S t) = fromText t
    build (I i) = fromText $ tshow i
    build (D d) = fromText $ tshow d

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
