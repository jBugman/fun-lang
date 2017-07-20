module Fun ( translate, translateFmt ) where

import ClassyPrelude

import Fun.Desugar    (desugar)
import Fun.Errors     (Error (..))
import Fun.Go.Printer (printGo)
import Fun.Parser     (parse)
import Go.Fmt         (gofmt)


translate :: Text -> Either Error Text
translate txt = parse txt >>= printGo . desugar

translateFmt :: Text -> Either Error Text
translateFmt txt = parse txt >>= printGo . desugar >>= gofmt
