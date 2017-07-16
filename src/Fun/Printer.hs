module Fun.Printer (singleLine) where

import ClassyPrelude
import Data.SCargot                 (SExprPrinter, basicPrint, encodeOne, setFromCarrier)
import Data.SCargot.Repr.WellFormed (fromWellFormed)

import Fun.SExpression (Atom (..), Expression)


singleLine :: Expression -> Text
singleLine = encodeOne printer

printer :: SExprPrinter Atom Expression
printer = setFromCarrier fromWellFormed (basicPrint printAtom)

printAtom :: Atom -> Text
printAtom (Ident s)   = s
printAtom (Keyword s) = s
printAtom (Type s)    = ":" <> s
printAtom (Op s)      = s
printAtom (Lit lit)   = tshow lit
