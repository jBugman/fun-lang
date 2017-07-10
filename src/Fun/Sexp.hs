{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module Fun.Sexp where

import ClassyPrelude
import Data.Text.Buildable
import Data.Text.Lazy.Builder (fromText)
import GHC.Err                (errorWithoutStackTrace)

data Expression a
    = Exp  [Expression a]
    | List [Expression a]
    | Op   a
    | Type a
    | Atom a
    | Unit
    deriving (Eq, Ord)

instance IsString (Expression Text) where
    fromString s
        | isOpChar s         = Op   (pack s)
        | ":" `isPrefixOf` s = Type (pack s)
        | otherwise          = Atom (pack s)
        where
            isOpChar :: String -> Bool
            isOpChar [c] = c `elem` opChars
            isOpChar _   = False

instance Buildable (Expression Text) where
    build (Atom s) = fromText s
    build (Type s) = fromText s
    build (Op s)   = fromText s
    build s        = errorWithoutStackTrace . unpack $ "Can only print terminal nodes, but got " <> tshow s

-- TODO: migrate to https://hackage.haskell.org/package/wl-pprint-text
instance Show (Expression Text) where
    show Unit      = "()"
    show (Atom s)  = unpack s
    show (Type s)  = unpack s
    show (Op s)    = unpack s
    show (List xs) = unpack $ mconcat ["[", showContents xs, "]"]
    show (Exp xs)  = unpack $ mconcat ["(", showContents xs, ")"]

showContents :: [Expression Text] -> Text
showContents xs = ointercalate " " $ fmap tshow xs -- TODO: add line-fold on long lists and some keywords

type instance Element (Expression a) = a

instance MonoFunctor (Expression Text) where
    omap _ Unit      = Unit
    omap f (Atom s)  = Atom $ f s
    omap f (Type s)  = Type $ f s
    omap f (Op s)    = Op   $ f s
    omap f (List xs) = List $ omap (omap f) xs
    omap f (Exp xs)  = Exp  $ omap (omap f) xs

opChars :: [Char]
opChars = "=+-*/<>%"
