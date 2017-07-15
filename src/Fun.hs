module Fun
    ( translate
    , SyntaxError (..)
) where

import ClassyPrelude
import Data.Either.Combinators (mapLeft)

import Fun.Go.Desugar (desugar)
import Fun.Go.Printer (SyntaxError (..), printPretty)
import Fun.Parser     (parse)


translate :: Text -> Either SyntaxError Text
translate txt = mapLeft SyntaxError (parse txt) >>= printPretty . desugar
