module Fun.Sexp where

import Data.List           (elem, intercalate, isPrefixOf, length)
import Data.String         (IsString, fromString)
import Data.Text           (Text, pack, unpack)
import Data.Text.Buildable (Buildable, build)

import qualified Data.Text.Lazy.Builder as B


data Expression
    = Exp  [Expression]
    | List [Expression]
    | Op   Text
    | Type Text
    | Atom Text
    | Unit
    deriving (Eq)

instance IsString Expression where
    fromString s
        | isOpChar s         = Op   (pack s)
        | ":" `isPrefixOf` s = Type (pack s)
        | otherwise          = Atom (pack s)
        where isOpChar cs = (length cs == 1) && (head cs `elem` opChars)

instance Buildable Expression where
    build (Atom s) = B.fromText s
    build (Type s) = B.fromText s
    build (Op s)   = B.fromText s
    build s        = errorWithoutStackTrace $ "Can only print terminal nodes, but got " ++ (show s)

instance Show Expression where
    show Unit      = "()"
    show (Atom s)  = unpack s
    show (Type s)  = unpack s
    show (Op s)    = unpack s
    show (List xs) = "[" ++ showContents xs ++ "]"
    show (Exp xs)  = "(" ++ showContents xs ++ ")"

showContents :: [Expression] -> String
showContents xs = intercalate " " $ map show xs -- TODO: add line-fold on long lists and some keywords

opChars :: String
opChars = "=+-*/<>%"
