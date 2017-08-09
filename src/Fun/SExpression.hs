{-# LANGUAGE PatternSynonyms #-}
module Fun.SExpression
    ( Expression (..)
    , Atom (..)
    , Literal (..)
    , unPos
    , pattern L
    , pattern A
    , pattern Nil
    , pattern LIT
    , pattern LT
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

import ClassyPrelude hiding (LT)

import qualified Data.Ord as Ord

import Fun.Errors (Pos (..))


data Expression
    = Atom !Atom        (Maybe Pos)
    | List [Expression] (Maybe Pos)
    deriving (Eq, Show)

unPos :: Expression -> Maybe Pos
unPos (Atom _ pos) = pos
unPos (List _ pos) = pos

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
    deriving (Eq, Ord, Show)

instance Ord Expression where
    compare (Atom _ _) (List _ _) = Ord.LT
    compare (List _ _) (Atom _ _) = Ord.GT
    compare (Atom x _) (Atom y _) = compare x y
    compare (List x _) (List y _) = compare x y

pattern Nil :: Expression
pattern Nil = List [] Nothing

pattern L :: [Expression] -> Expression
pattern L xs = List xs Nothing

pattern A :: Atom -> Expression
pattern A x = Atom x Nothing

pattern LIT :: Literal -> Maybe Pos -> Expression
pattern LIT x p = Atom (Literal x) p

pattern LT :: Literal -> Expression
pattern LT x = Atom (Literal x) Nothing

pattern ID :: Text -> Expression
pattern ID x = A (Ident x)

pattern KW :: Text -> Expression
pattern KW x = A (Keyword x)

pattern TP :: Text -> Expression
pattern TP x = A (Type x)

pattern OP :: Text -> Expression
pattern OP x = A (Operator x)

pattern SL :: Text -> Maybe Pos -> Expression
pattern SL x p = LIT (String x) p

pattern CL :: Text -> Maybe Pos -> Expression
pattern CL x p = LIT (Char x) p

pattern I :: Integer -> Expression
pattern I x = A (Literal (Integer 10 x))

pattern INT :: Int -> Integer -> Expression
pattern INT base x = A (Literal (Integer base x))

pattern DL :: Double -> Maybe Pos -> Expression
pattern DL x p = LIT (Double x) p

pattern BL :: Bool -> Maybe Pos -> Expression
pattern BL x p = LIT (Bool x) p
