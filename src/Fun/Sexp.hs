{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module Fun.Sexp where

import ClassyPrelude
import Data.Semigroup         ((<>))
import Data.Text.Buildable
import Data.Text.Lazy.Builder (fromText)
import GHC.Err                (errorWithoutStackTrace)

data Expression a
    = Exp  [Expression a]
    | List [Expression a]
    | Op   a
    | Type a
    | Atom a -- TODO: pack everything atomic-like into Atom
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

instance Semigroup (Expression Text) where
    (<>) (Atom x) (Atom y)   = Exp [Atom x, Atom y]
    (<>) (Atom x) (Op y)     = Exp [Atom x, Op y]
    (<>) (Atom x) (Exp xs)   = Exp [Atom x, Exp xs]
    (<>) (Atom x) (List xs)  = Exp [Atom x, List xs]
    (<>) (Op x) (Atom y)     = Exp [Op x, Atom y]
    (<>) (Op x) (Op y)       = Exp [Op x, Op y] -- not really a valid case
    (<>) (Op x) (List xs)    = Exp [Op x, List xs]
    (<>) (List xs) (List ys) = List $ xs <> ys
    (<>) (List xs) x         = List $ xs `snoc` x
    (<>) (Exp xs) (Exp ys)   = Exp $ xs <> ys
    (<>) (Exp xs) x          = Exp $ xs `snoc` x
    -- (<>) x y                = Exp [x, y]

instance Monoid (Expression Text) where
    mempty = Unit
    mappend (Atom x) Unit = Exp [Atom x, Unit]
    -- mappend (Atom x) (Atom y) = Exp [x, y]

-- instance MonoFoldable (Expression Text) where
-- --  -- ofoldMap :: Monoid m => (Text -> m) -> Expression Text -> m
--     -- ofoldMap _ Unit = Unit
--     ofoldMap f = oconcat . omap f
--     ofoldr _ x Unit = x

-- instance MonoTraversable (Expression Text) where
--     otraverse _ Unit     = Unit
--     otraverse f (Atom s) = Atom $ f s
--     -- fmap f (Type s)  = Type $ f s
--     -- fmap f (Op s)    = Op   $ f s
--     -- fmap f (List xs) = List $ fmap (fmap f) xs
--     -- fmap f (Exp xs)  = Exp  $ fmap (fmap f) xs

opChars :: [Char]
opChars = "=+-*/<>%"
