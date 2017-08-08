module Fun ( translate, translateFmt ) where

import ClassyPrelude

import Fun.Desugar    (desugar)
import Fun.Errors     (Error (..))
import Fun.Go.Printer (printGo)
import Go.Fmt         (gofmt)
import Go.Parser      (parse)


translate :: Text -> Either Error Text
translate txt = parse txt >>= printGo . desugar

translateFmt :: Text -> Either Error Text
translateFmt txt = parse txt >>= printGo . desugar >>= gofmt
