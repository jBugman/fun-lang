module Fun.Errors where

import ClassyPrelude (Eq, Show, Text, (<>))


data Error
    = SyntaxError      Text
    | TranslationError Text
    | GoError          Text
    deriving (Eq, Show)

unError :: Error -> Text
unError (SyntaxError err)      = "syntax error: "      <> err
unError (TranslationError err) = "translation error: " <> err
unError (GoError err)          = "Go error: "          <> err

