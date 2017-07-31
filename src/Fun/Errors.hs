module Fun.Errors where

import ClassyPrelude

type Pos = (Int, Int)

data Error
    = SyntaxError      (Maybe Pos) Text
    | TranslationError (Maybe Pos) Text
    | GoError          (Maybe Pos) Text
    deriving (Eq, Show)

unError :: Error -> Text
unError (SyntaxError (Just (line, col)) err)
    = tshow line <> ":" <> tshow col <> ": syntax error: " <> err
unError (SyntaxError _ err)
    = "syntax error: " <> err

unError (TranslationError (Just (line, col)) err)
    = tshow line <> ":" <> tshow col <> ": translation error: " <> err
unError (TranslationError _ err)
    = "translation error: " <> err

unError (GoError (Just (line, col)) err)
    = tshow line <> ":" <> tshow col <> ": Go error: " <> err
unError (GoError _ err)
    = "Go error: "          <> err
