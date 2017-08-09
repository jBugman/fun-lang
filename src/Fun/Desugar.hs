{-# LANGUAGE PatternSynonyms #-}
module Fun.Desugar (desugar) where

import ClassyPrelude
import Data.Traversable (mapAccumR)
import GHC.Err          (errorWithoutStackTrace)

import Fun.SExpression (pattern A, Expression (..), pattern ID, pattern KW, pattern L, pattern LIT,
                        Literal (..), pattern OP, unPos)

type E = Expression

desugar :: E -> E
desugar (L (KW "package" : ID name p : topLevels))
    = L $ [ KW "package", ID name p ] <> imports <> decls
    where
        (imports, decls) = swapWith addReturn
                         . swapPrintf
                         . swapPrint
                         . span isImport $ topLevels

        swapPrint  = swapAddImport importFmt (eq (KW "print")) (ID "fmt.Println" Nothing)
        swapPrintf = swapAddImport importFmt (eq (KW "printf")) (ID "fmt.Printf" Nothing)

desugar e = e -- Ignore non-package

-- TODO: add more eq checks when needed
eq :: Expression -> Expression -> Bool
eq (KW x) (KW y) = x == y
eq _ _           = False

importFmt :: E
importFmt = L [ KW "import" , LIT (String "fmt") Nothing ]
-- TODO: Probably a problem with inserting expressions without position.


isImport :: E -> Bool
isImport (L (KW "import" : _)) = True
isImport _                     = False


swapAddImport :: E -> (E -> Bool) -> E -> ([E], [E]) -> ([E], [E])
swapAddImport imp cond to (imports, decls) = mapAccumR go imports decls
    where
        go :: [E] -> E -> ([E], E)
        go imps ex
            | cond ex    = (addImport imps, updatePos ex to)
            | L xs <- ex = L <$> mapAccumR go imps xs
            | otherwise  = (imps, ex)

        addImport :: [E] -> [E]
        addImport xs = ordNub $ imp : xs

updatePos :: Expression -> Expression -> Expression
updatePos src (Atom x _)  = Atom x (unPos src)
updatePos src (List xs _) = List xs (unPos src)

swapWith :: (E -> E) -> ([E], [E]) -> ([E], [E])
swapWith f (imports, decls) = (imports, fmap f decls)

addReturn :: E -> E
addReturn (L [ KW "func" , name , args , results , body ])
    = L [ KW "func" , name , args , results , withReturn body ]
addReturn (L [ KW "method" , recv , name , args , results , body ])
    = L [ KW "method" , recv , name , args , results , withReturn body ]
addReturn (L xs)                                             = L (addReturn <$> xs)
addReturn ex                                                 = ex

withReturn :: E -> E
withReturn (A a)                      = L [KW "return" , A a ]
withReturn ret@(L (KW "return" : _ )) = ret
withReturn b
    | isExpression b = L [KW "return" , b ]
    | L exprs <- b   = case unsnoc exprs of
        Just (xs, x) -> L $ xs `snoc` withReturn x
    -- Throwing exceptions here for to catch edge cases.
    -- These conditions should not be met.
        Nothing      -> errorWithoutStackTrace ("addReturn: " <> show b)
    | otherwise      =  errorWithoutStackTrace ("addReturn: " <> show b)


isExpression :: E -> Bool
isExpression (Atom _ _)                 = True
isExpression (List ( OP _ _ : _ ) _)    = True
isExpression (List ( ID _ _ : _ ) _)    = True
isExpression (List ( KW "val"  : _ ) _) = True
isExpression (List ( KW "func" : _ ) _) = True
isExpression _                          = False
