{-# LANGUAGE PatternSynonyms #-}
module Fun.SExpression
    ( Expression (..)
    , Atom (..)
    , Literal (..)
    , pattern L
    , pattern A
    , pattern Nil
    , pattern SL
    , pattern CL
    , pattern I
    , pattern INT
    , pattern DL
    , pattern BL
    , pattern ID
    , pattern TP
    , pattern OP
    , pattern KW
) where

import ClassyPrelude
import Numeric       (showHex, showOct)

import qualified Text.PrettyPrint.Leijen.Text as PP


data Expression
    = Atom !Atom
    | List [Expression]
    deriving (Eq, Ord, Show)

data Atom
    = Ident    !Text
    | Keyword  !Text
    | Type     !Text
    | Operator !Text
    | Literal  !Literal
    deriving (Eq, Ord, Show)

data Literal
    = String  !Text
    | Char    !Text
    | Integer !Int !Integer
    | Double  !Double
    | Bool    !Bool
    deriving (Eq, Ord)

pattern Nil :: Expression
pattern Nil = List []

pattern L :: [Expression] -> Expression
pattern L xs = List xs

pattern A :: Atom -> Expression
pattern A x = Atom x

pattern ID :: Text -> Expression
pattern ID x = A (Ident x)

pattern KW :: Text -> Expression
pattern KW x = A (Keyword x)

pattern TP :: Text -> Expression
pattern TP x = A (Type x)

pattern OP :: Text -> Expression
pattern OP x = A (Operator x)

pattern SL :: Text -> Expression
pattern SL x = A (Literal (String x))

pattern CL :: Text -> Expression
pattern CL x = A (Literal (Char x))

pattern I :: Integer -> Expression
pattern I x = A (Literal (Integer 10 x))

pattern INT :: Int -> Integer -> Expression
pattern INT base x = A (Literal (Integer base x))

pattern DL :: Double -> Expression
pattern DL x = A (Literal (Double x))

pattern BL :: Bool -> Expression
pattern BL x  = A (Literal (Bool x))

instance PP.Pretty Literal where
    pretty (String x)     = PP.dquotes . PP.textStrict $ x
    pretty (Char x)       = PP.squotes . PP.textStrict $ x
    pretty (Integer 16 x) = PP.text . pack $ "0x" <> showHex x ""
    pretty (Integer 8 x)  = PP.text . pack $ "0"  <> showOct x ""
    pretty (Integer _ x)  = PP.integer x -- 10 or 0 are supported
    pretty (Double x)     = PP.double x
    pretty (Bool True)    = PP.text "true"
    pretty (Bool False)   = PP.text "false"

instance Show Literal where
    show = show . PP.pretty
