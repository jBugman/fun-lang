module Fun.Main
    ( translate
    , translate'
    ) where

import Data.Either.Combinators (mapBoth)
import Data.Text.Lazy

import Fun.Go.Desugar (desugar)
import Fun.Go.Printer (SyntaxError (..), printPretty)
import Fun.Parser     (prettyError, prs, sexp)


translate :: Text -> Either SyntaxError Text
translate txt = case prs sexp txt of
    Left err -> Left . SyntaxError . prettyError $ err
    Right se -> printPretty . desugar $ se

translate' :: String -> Either SyntaxError String
translate' txt = mapBoth id unpack (translate $ pack txt)
