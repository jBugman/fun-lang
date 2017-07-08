{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
module Fun.Sexp where

import Data.List           (elem, isPrefixOf, length)
import Data.String         (IsString, fromString, unwords)
import Data.Text           (Text, pack, unpack)
import Data.Text.Buildable (Buildable, build)
import Prelude             hiding (unwords)

import qualified Data.Text.Lazy.Builder as B


data Expression a
    = Exp  [Expression a]
    | List [Expression a]
    | Op   a
    | Type a
    | Atom a
    | Unit
    deriving (Eq, Functor)

instance IsString (Expression Text) where
    fromString s
        | isOpChar s         = Op   (pack s)
        | ":" `isPrefixOf` s = Type (pack s)
        | otherwise          = Atom (pack s)
        where isOpChar cs = (length cs == 1) && (head cs `elem` opChars)

instance Buildable (Expression Text) where
    build (Atom s) = B.fromText s
    build (Type s) = B.fromText s
    build (Op s)   = B.fromText s
    build s        = errorWithoutStackTrace $ "Can only print terminal nodes, but got " ++ show s

instance Show (Expression Text) where
    show Unit      = "()"
    show (Atom s)  = unpack s
    show (Type s)  = unpack s
    show (Op s)    = unpack s
    show (List xs) = "[" ++ showContents xs ++ "]"
    show (Exp xs)  = "(" ++ showContents xs ++ ")"

showContents :: [Expression Text] -> String
showContents xs = unwords $ map show xs -- TODO: add line-fold on long lists and some keywords

-- instance Functor Expression where
--     fmap _ Unit      = Unit
--     fmap f (Atom s)  = Atom $ f s
--     fmap f (Type s)  = Type $ f s
--     fmap f (Op s)    = Op   $ f s
--     fmap f (List xs) = List $ fmap (fmap f) xs
--     fmap f (Exp xs)  = Exp  $ fmap (fmap f) xs


opChars :: String
opChars = "=+-*/<>%"
