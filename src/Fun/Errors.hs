module Fun.Errors
    ( Error (..)
    , Pos (..)
    , unError
) where

import ClassyPrelude

data Pos = Pos Int Int
    deriving (Eq)

instance Show Pos where
    show (Pos line col) = show line <> ":" <> show col <> ":"

data Error
    = SyntaxError      (Maybe Pos) Text
    | TranslationError (Maybe Pos) Text
    | GoError          (Maybe Pos) Text
    deriving (Eq, Show)

printPos :: Maybe Pos -> Text
printPos (Just p) = tshow p <> " "
printPos Nothing  = ""

unError :: Error -> Text
unError (SyntaxError pos err)
    = printPos pos <> "syntax error: " <> err

unError (TranslationError pos err)
    = printPos pos <> "translation error: " <> err

unError (GoError pos err)
    = printPos pos <> "Go error: "          <> err
