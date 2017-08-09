{-# LANGUAGE PatternSynonyms #-}
module Fun.PrettyPrinter (singleLine) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (pattern A, pattern L)
import Text.PrettyPrint.Leijen.Text (Doc, displayTStrict, parens, renderOneLine, sep, textStrict)

import Fun.SExpression (Atom (..), Expression)


singleLine :: Expression -> Text
singleLine x = displayTStrict . renderOneLine $ pprint x
-- displayTStrict . renderPretty 0.6 100 <$> pprint x

pprint :: Expression -> Doc
pprint (L xs) = parens . sep $ pprint <$> xs
pprint (A x)  = pa x
pprint e      = error $ "pretty print: should not be: " <> show e

pa :: Atom -> Doc
pa (Ident s)   = textStrict s
pa (Keyword s) = textStrict s
pa (Type s)    = textStrict (":" <> s)
pa (Op s)      = textStrict s
pa (Lit lit)   = textStrict (tshow lit)
