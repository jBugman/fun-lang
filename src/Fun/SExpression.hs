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
    deriving (Eq, Ord)

instance Show Atom where
    show (Ident s) = unpack s
    show (Type s)  = unpack s
    show (Op s)    = unpack s

instance IsString Atom where
    fromString s
        | isOpChar s         = Op    (pack s)
        | ":" `isPrefixOf` s = Type  (pack s)
        | otherwise          = Ident (pack s)
        where
            isOpChar :: String -> Bool
            isOpChar [c] = c `elem` opChars
            isOpChar _   = False

instance Buildable Expression where
    build (WFSAtom x) = case x of
        (Ident s) -> fromText s
        (Type s)  -> fromText s
        (Op s)    -> fromText s
    build (WFSList x) = errorWithoutStackTrace $ unpack ("Can only print terminal nodes, but got " <> tshow x)

-- TODO: migrate to https://hackage.haskell.org/package/wl-pprint-text
-- instance Show Expression where
--     show (List xs) = unpack $ mconcat ["(", showContents xs, ")"]
--     show (Ident s) = unpack s
--     show (Type s)  = unpack s
--     show (Op s)    = unpack s

-- showContents :: [Expression] -> Text
-- showContents xs = ointercalate " " $ fmap tshow xs -- TODO: add line-fold on long lists and some keywords

opChars :: [Char]
opChars = "=+-*/<>%"
