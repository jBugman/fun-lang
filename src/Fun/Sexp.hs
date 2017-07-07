module Fun.Sexp where

import Data.List           (elem, length)
import Data.String         (IsString, fromString)
import Data.Text           (Text, pack)
import Data.Text.Buildable (Buildable, build)

import qualified Data.Text.Lazy.Builder as B


data Expression
    = Exp  [Expression]
    | List [Expression]
    | Op   Text
    | Atom Text
    | Unit
        deriving (Eq, Show)

instance IsString Expression where
    fromString s
        | isOpChar s = Op (pack s)
        | otherwise  = Atom (pack s)
        where isOpChar cs = (length cs == 1) && (head cs `elem` opChars)

instance Buildable Expression where
    build (Atom s) = B.fromText s
    build (Op s)   = B.fromText s
    build s        = B.fromString . show $ s

opChars :: String
opChars = "=+-*/<>"
