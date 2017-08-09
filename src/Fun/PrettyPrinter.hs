{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fun.PrettyPrinter (singleLine) where

import ClassyPrelude
import Numeric                      (showHex, showOct)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty, displayTStrict, double, dquotes, integer, parens,
                                     pretty, renderOneLine, sep, squotes, textStrict)

import Fun.SExpression (Atom (..), Expression (..), Literal (..))


singleLine :: Expression -> Text
singleLine x = displayTStrict . renderOneLine $ pprint x
-- displayTStrict . renderPretty 0.6 100 <$> pprint x

pprint :: Expression -> Doc
pprint (List xs _)            = parens . sep $ pprint <$> xs
pprint (Atom (Ident s) _)     = textStrict s
pprint (Atom (Keyword s) _)   = textStrict s
pprint (Atom (Type s) _)      = textStrict (":" <> s)
pprint (Atom (Operator s) _)  = textStrict s
pprint (Atom (Literal lit) _) = pretty lit

instance Pretty Literal where
    pretty (String x)     = dquotes . textStrict $ x
    pretty (Char x)       = squotes . textStrict $ x
    pretty (Integer 16 x) = textStrict . pack $ "0x" <> showHex x ""
    pretty (Integer 8 x)  = textStrict . pack $ "0"  <> showOct x ""
    pretty (Integer _ x)  = integer x -- 10 or 0 are supported
    pretty (Double x)     = double x
    pretty (Bool True)    = textStrict "true"
    pretty (Bool False)   = textStrict "false"
