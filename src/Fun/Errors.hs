module Fun.Errors
    ( Error (..)
    , Pos (..)
    , unError
) where

import ClassyPrelude
import Data.Text     (strip)

data Pos = Pos Int Int
    deriving (Eq, Ord)

instance Show Pos where
    show (Pos line col) = show line <> ":" <> show col <> ":"

data Error
    = SyntaxError      (Maybe Pos) Text
    | TranslationError (Maybe Pos) Text
    | GoError          (Maybe Pos) Text
    deriving (Eq, Show)

printPos :: Maybe Pos -> Text
printPos (Just p) = tshow p
printPos Nothing  = ""

unError :: Error -> Text
unError (SyntaxError pos err)
    = strip $ printPos pos <> " syntax error: " <> err

unError (TranslationError pos err)
    = strip $ printPos pos <> " translation error: " <> err

unError (GoError pos err)
    = strip $ printPos pos <> " Go error: " <> err
