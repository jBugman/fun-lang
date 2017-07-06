module Fun.Sexp where

import Data.String         (IsString, fromString)
import Data.Text           (Text, pack)
import Data.Text.Buildable

import qualified Data.Text.Lazy.Builder as B


data Expression = Exp [Expression] | List [Expression] | Atom Text | Unit
    deriving (Eq, Show)

instance IsString Expression where
    fromString = Atom . pack

instance Buildable Expression where
    build (Atom s) = B.fromText s
    build s        = B.fromString . show $ s
