module Fun ( translate ) where

import ClassyPrelude

import Fun.Errors     (Error (..))
import Fun.Go.Desugar (desugar)
import Fun.Go.Printer (printPretty)
import Fun.Parser     (parse)


translate :: Text -> Either Error Text
translate txt = parse txt >>= printPretty . desugar
