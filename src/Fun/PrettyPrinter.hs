{-# LANGUAGE PatternSynonyms #-}
module Fun.PrettyPrinter (singleLine) where

import ClassyPrelude
import Text.PrettyPrint.Leijen.Text (Doc, displayTStrict, parens, renderOneLine, sep, textStrict)

import Fun.SExpression (Atom (..), Expression (..))


singleLine :: Expression -> Text
singleLine x = displayTStrict . renderOneLine $ pprint x
-- displayTStrict . renderPretty 0.6 100 <$> pprint x

pprint :: Expression -> Doc
pprint (List xs)            = parens . sep $ pprint <$> xs
pprint (Atom (Ident s))     = textStrict s
pprint (Atom (Keyword s))   = textStrict s
pprint (Atom (Type s))      = textStrict (":" <> s)
pprint (Atom (Operator s))  = textStrict s
pprint (Atom (Literal lit)) = textStrict (tshow lit)
