module Fun ( translate ) where

import ClassyPrelude

import Fun.Desugar    (desugar)
import Fun.Errors     (Error (..))
import Fun.Go.Printer (printPretty)
import Fun.Parser     (parse)


translate :: Text -> Either Error Text
translate txt = parse txt >>= printPretty . desugar
