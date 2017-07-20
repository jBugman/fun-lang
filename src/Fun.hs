module Fun ( translate, translateFmt ) where

import ClassyPrelude hiding (print)

import Fun.Desugar    (desugar)
import Fun.Errors     (Error (..))
import Fun.Go.Printer (print)
import Fun.Parser     (parse)
import Go.Fmt         (gofmt)


translate :: Text -> Either Error Text
translate txt = parse txt >>= print . desugar

translateFmt :: Text -> Either Error Text
translateFmt txt = do
    ex <- parse txt
    s <- print . desugar $ ex
    gofmt s
