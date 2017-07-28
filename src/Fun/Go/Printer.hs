{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Printer ( printGo ) where

import ClassyPrelude                hiding (empty)
import Data.SCargot.Repr.WellFormed (pattern A, pattern L, pattern Nil)
import Text.PrettyPrint.Leijen.Text (Doc, braces, brackets, colon, comma, displayTStrict, dot,
                                     empty, equals, hsep, indent, line, parens, pretty, punctuate,
                                     renderPretty, semi, text, textStrict, vsep, (<+>))

import Fun.Errors        (Error (..))
import Fun.PrettyPrinter (singleLine)
import Fun.SExpression   (Atom (..), Expression, pattern ID, pattern KW, pattern OP, pattern SL,
                          pattern TP)

type E = Expression

printGo :: E -> Either Error Text
printGo x = displayTStrict . renderPretty 0.6 100 <$> pprint x


pprint :: E -> Either Error Doc
-- ident
pprint (ID x)      = doc x

-- operator
pprint (OP x)      = doc x

-- literal
pprint (A (Lit x)) = pure $ pretty x

-- types
pprint (TP "any")  = doc "interface{}"
pprint (TP x)      = doc x

pprint (L [ TP "ptr" , x ]) = do
    x' <- pprint x
    pure $ text "*" <> x'

pprint (L [ TP "slice" , x ]) = do
    x' <- pprint x
    pure $ text "[]" <> x'

pprint (L [ TP "map" , k@(TP _) , v ]) = do
    k' <- pprint k
    v' <- pprint v
    pure $ text "map" <> brackets k' <> v'

pprint (L [ TP "func" , a ]) = do
    a' <- printArgs a
    pure $ text "func" <> parens a'

pprint (L [ TP "func" , a , r ]) = do
    a' <- printArgs a
    r' <- pprint r
    pure $ text "func" <> parens a' <+> r'

-- type alias
pprint (L [ KW "alias" , n@(ID _) , t ]) = do
    n' <- pprint n
    t' <- pprint t
    pure $ text "type" <+> n' <+> t'

-- const
pprint (L [ KW "const" , x@(ID _) , rhs ])
    = printAssignmentX kwConst x Nothing (Just rhs)

pprint (L [ KW "const" , x@(ID _) , t@(TP _) , rhs ])
    = printAssignmentX kwConst x (Just t) (Just rhs)

-- var
pprint (L [ KW "var" , x@(ID _) , t@(TP _) ])
    = printAssignmentX kwVar x (Just t) Nothing

pprint (L [ KW "var" , x@(ID _) , rhs@(L [ TP _ , L _ ]) ])
    = printAssignmentX kwVar x Nothing (Just rhs)

pprint (L [ KW "var" , x@(ID _) , rhs@(L [ L( TP _ : _ ) , L _ ]) ])
    = printAssignmentX kwVar x Nothing (Just rhs)

pprint (L [ KW "var" , x@(ID _) , t@( L( TP _ : _ )) ])
    = printAssignmentX kwVar x (Just t) Nothing

pprint (L [ KW "var" , x@(ID _) , y@(ID _) , rhs ])
    = printAssignmentXY kwVar x y rhs

pprint (L [ KW "var" , x@(ID _) , t@(TP _) , rhs ])
    = printAssignmentX kwVar x (Just t) (Just rhs)

pprint (L [ KW "var" , x@(ID _) , rhs ])
    = printAssignmentX kwVar x Nothing (Just rhs)

-- struct decl
pprint (L [ KW "struct" , n@(ID _) ]) = do
    n' <- pprint n
    pure $ text "type" <+> n' <+> text "struct" <> emptyBraces

pprint (L ( KW "struct" : n@(ID _) : xs )) = do
    n'  <- pprint n
    xs' <- mapM printStructElem xs
    pure $ text "type" <+> n' <+> text "struct"
        <+> bracedBlock (vsep xs')

-- interface decl
pprint (L [ KW "interface" , n@(ID _) ]) = do
    n'  <- pprint n
    pure $ text "type" <+> n' <+> text "interface" <> emptyBraces

pprint (L ( KW "interface" : n@(ID _) : xs )) = do
    n'  <- pprint n
    xs' <- mapM printInterfaceElem xs
    pure $ text "type" <+> n' <+> text "interface"
        <+> bracedBlock (vsep xs')

-- func
pprint (L [ KW "func" , n@(ID _) , b ]) = do
    n' <- pprint n
    b' <- printBody b
    pure $ text "func" <+> n' <> parens empty <+> b'

pprint (L [ KW "func" , n@(ID _) , a , b ]) = do
    n' <- pprint n
    a' <- printArgs a
    b' <- printBody b
    pure $ text "func" <+> n' <> parens a' <+> b'

pprint (L [ KW "func" , n@(ID _) , a , r , b ]) = do
    n' <- pprint n
    a' <- printArgs a
    r' <- printResults r
    b' <- printBody b
    pure $ text "func" <+> n' <> parens a' <+> r' <+> b'

-- method
pprint (L [ KW "method" , r , n@(ID _) , b ]) = do
    r' <- printRecv r
    n' <- pprint n
    b' <- printBody b
    pure $ text "func" <+> r' <+> n' <> parens empty <+> b'

pprint (L [ KW "method" , r , n@(ID _) , a , b ]) = do
    r' <- printRecv r
    n' <- pprint n
    a' <- printArgs a
    b' <- printBody b
    pure $ text "func" <+> r' <+> n' <> parens a' <+> b'

pprint (L [ KW "method" , r , n@(ID _) , a , rs , b ]) = do
    r'  <- printRecv r
    n'  <- pprint n
    a'  <- printArgs a
    rs' <- printResults rs
    b'  <- printBody b
    pure $ text "func" <+> r' <+> n' <> parens a' <+> rs' <+> b'

-- unary operators
pprint (L [ op@(OP _) , x ]) = do
    op' <- pprint op
    x'  <- pprint x
    pure $ case op of
        OP "++" -> x'  <> op'
        OP "--" -> x'  <> op'
        _       -> op' <> x'

-- binary operators
pprint (L [ OP "=" , lhs , rhs ]) = do -- TODO: change to ==
    lhs' <- pprint lhs
    rhs' <- pprint rhs
    pure $ lhs' <+> text "==" <+> rhs'

pprint (L ( op@(OP _) : xs )) = do
    op' <- pprint op
    xs' <- mapM pprint xs
    pure $ hsep (intersperse op' xs')

-- assignment
pprint (L [ KW "set" , x , rhs ]) = do
    x'   <- pprint x
    rhs' <- pprint rhs
    pure $ x' <+> equals <+> rhs'

-- pprint (L [ KW "set" , ID "_", x , xs ])
--     = printf2 "_, {} = {}" <$> print x <*> print xs

pprint (L [ KW "set" , x@(ID _) , y@(ID _) , rhs ]) = do
    x'   <- pprint x
    y'   <- pprint y
    rhs' <- pprint rhs
    pure $ x' <> comma <+> y' <+> equals <+> rhs'

-- import
pprint (L [ KW "import" , x@(SL _) ]) = do
    x' <- pprint x
    pure $ text "import" <+> x'

pprint (L [ KW "import" , x@(SL _) , SL alias ]) = do
    x' <- pprint x
    pure $ text "import" <+> textStrict alias <+> x'

-- return
pprint (L ( KW "return" : xs ) ) = do
    xs' <- mapM pprint xs
    pure $ text "return" <+> commaSep xs'

-- range
pprint (L [ KW "range" , x@(ID _) , xs ]) = do
    x'  <- pprint x
    xs' <- pprint xs
    pure $ x' <+> text ":=" <+> text "range" <+> xs'

pprint (L [ KW "range" , x@(ID _) , y@(ID _) , xs ]) = do
    x'  <- pprint x
    y'  <- pprint y
    xs' <- pprint xs
    pure $ x' <> comma <+> y' <+> text ":=" <+> text "range" <+> xs'

-- indexed access
pprint (L [ KW "val" , n@(ID _) , i ]) = do
    n' <- pprint n
    i' <- pprint i
    pure $ n' <> brackets i'

pprint (L [ KW "val" , n@(ID _) , i , j ]) = do
    n' <- pprint n
    i' <- pprint i
    j' <- pprint j
    pure $ n' <> brackets i' <> brackets j'

-- slice literal
pprint (L [ t@(L [ TP "slice" , _ ]) , L xs ]) = do
    t'  <- pprint t
    xs' <- mapM pprint xs
    pure $ t' <> braces (commaSep xs')

-- map literal
pprint (L [ t@(L [ TP "map" , _ , _ ]) , L xs ]) = printMapLike t xs

-- struct literal
pprint (L [ t@(TP _) ]) = do
    t' <- pprint t
    pure $ t' <> emptyBraces

pprint (L [ t@(TP _) , L xs ]) = printMapLike t xs

-- function call
pprint (L [ f@(ID _) ]) = do
    f' <- pprint f
    pure $ f' <> text "()"

pprint (L ( f@(ID _) : a )) = do
    f' <- pprint f
    a' <- mapM pprint a
    pure $ f' <> parens (commaSep a')

-- make
pprint (L [ KW "make" , s@(L ( TP "slice" : _ )) , d ]) = do
    s' <- pprint s
    d' <- pprint d
    pure $ text "make" <> parens (s'<> comma <+> d')

pprint (L [ KW "make" , m@(L [ TP "map" , _ , _ ]) ]) = do
    m' <- pprint m
    pure $ text "make" <> parens m'

-- type casting
pprint (L [ KW "cast" , t@(TP _) , x ]) = do
    t' <- pprint t
    x' <- pprint x
    pure $ t' <> parens x'

pprint (L [ KW "cast" , t@(L (TP _ : _ )) , x ])= do
    t' <- pprint t
    x' <- pprint x
    pure $ t' <> parens x'

-- type assertion
pprint (L [ KW "assert" , t@(TP _) , x ])= do
    t' <- pprint t
    x' <- pprint x
    pure $ x' <> dot <> parens t'

-- lambda
pprint (L [ KW "func" , Nil , b ]) = do
    b' <- printBody b
    pure $ text "func" <> parens empty <+> b'

pprint (L [ KW "func" , a , b ]) = do
    a' <- printArgs a
    b' <- printBody b
    pure $ text "func" <> parens a' <+> b'

pprint (L [ KW "func" , a , r , b ]) = do
    a' <- printArgs a
    r' <- printResults r
    b' <- printBody b
    pure $ text "func" <> parens a' <+> r' <+> b'

-- if-then-else
pprint (L [ KW "if" , c , t ]) = printIfThen c t

-- elseif
pprint (L [ KW "if" , c , t , e@(L ( KW "if" : _ )) ]) = do
    it <- printIfThen c t
    e' <- pprint e
    pure $ it <+> text "else" <+> e'

pprint (L [ KW "if" , c , t , e ]) = do
    it <- printIfThen c t
    e' <- pprint e
    pure $ it <+> text "else" <+> bracedBlock e'

-- for loop
pprint (L [ KW "for" , body ]) = do
    body' <- pprint body
    pure $ text "for" <+> bracedBlock body'

pprint (L [ KW "for" , cond , body ]) = do
    cond' <- pprint cond
    body' <- pprint body
    pure $ text "for" <+> cond' <+> bracedBlock body'

pprint (L [ KW "for" , i@(ID _) , from , cond , iter , body ])
    = printFor i from cond iter body

pprint (L [ KW "for" , i@(ID _) , from , to , body ])
    = printFor i from (L [ OP "<" , i , to ]) (L [ OP "++" , i ]) body

-- package
pprint (L ( KW "package" : n@(ID _) : topLevels )) = do
    n'  <- pprint n
    ts' <- mapM pprint topLevels
    pure $ vsep . punctuate line $ (text "package" <+> n') : ts'

-- slicing
pprint (L [ KW "slice" , x@(ID _) , ID "_" , to ]) = do
    x'  <- pprint x
    to' <- pprint to
    pure $ x' <> brackets ( colon <> to' )

pprint (L [ KW "slice" , x@(ID _) , from , ID "_" ]) = do
    x'    <- pprint x
    from' <- pprint from
    pure $ x' <> brackets ( from' <> colon )

pprint (L [ KW "slice" , x@(ID _) , from , to ]) = do
    x'    <- pprint x
    from' <- pprint from
    to'   <- pprint to
    pure $ x' <> brackets ( from' <> colon <> to' )

-- Bare keyword (continue e.t.c.)
pprint (L [ KW x ]) = doc x

-- Recursive expression list
pprint (L [ x@(L _) ] )     = pprint x
pprint (L ( x@(L _) : xs )) = do
    x'  <- pprint x
    xs' <- pprint (L xs)
    pure $ x' <> line <> xs'

-- FIXME: Catching everything that is not allowed (yet)
pprint x = Left . TranslationError $ "not supported yet: " <> singleLine x


-- Tier 2 --

printFor :: E -> E -> E -> E -> E -> Either Error Doc
printFor var from cond iter body = do
    var'  <- pprint var
    from' <- pprint from
    cond' <- pprint cond
    iter' <- pprint iter
    body' <- pprint body
    pure $ text "for"
        <+> var' <+> text ":=" <+> from' <> semi
        <+> cond' <> semi
        <+> iter'
        <+> bracedBlock body'

printRecv :: E -> Either Error Doc
printRecv t@(TP _) = do
    t' <- pprint t
    pure $ parens t'
printRecv (L [ n@(ID _) , t ]) = do
    n' <- pprint n
    t' <- pprint t
    pure $ parens (n' <+> t')
printRecv e = mkError "invalid reciever: " e

printArgs :: E -> Either Error Doc
printArgs Nil    = pure empty
printArgs (L xs) = commaSep <$> mapM printArg xs
printArgs e      = mkError "invalid args: " e

printArg :: E -> Either Error Doc
printArg t@(TP _)             = pprint t
printArg t@(L (TP _ : _))     = pprint t
printArg (L [ n@(ID _) , t ]) = do
    n' <- pprint n
    t' <- pprint t
    pure $ n' <+> t'
printArg e                    = mkError "invalid arg: " e

printResults :: E -> Either Error Doc
printResults t@(TP _) = pprint t
printResults t@(L ( TP "func" : _ )) = pprint t
printResults (L ts)  = do
    ts' <- mapM pprint ts
    pure $ parens (commaSep ts')
printResults e        = mkError "invalid results: " e

printBody :: E -> Either Error Doc
printBody Nil = pure emptyBraces
printBody xs  = bracedBlock <$> pprint xs

printStructElem :: E -> Either Error Doc
printStructElem (L [ x@(ID _) , y ]) = do
    x' <- pprint x
    y' <- pprint y
    pure $ x' <+> y'
printStructElem e = mkError "invalid struct elem: " e

printInterfaceElem :: E -> Either Error Doc
printInterfaceElem (L [ n@(ID _) , a ]) = do
    n' <- pprint n
    a' <- printArgs a
    pure $ n' <> parens a'
printInterfaceElem (L [ n@(ID _) , a , r ]) = do
    n' <- pprint n
    a' <- printArgs a
    r' <- printResults r
    pure $ n' <> parens a' <+> r'
printInterfaceElem e = mkError "invalid interface elem: " e

printMapLike :: E -> [E] ->  Either Error Doc
printMapLike t xs = do
    t'  <- pprint t
    xs' <- mapM printMaplikeElem xs
    pure $ t' <> braces (commaSep xs')

printMaplikeElem :: E -> Either Error Doc
printMaplikeElem (L [ x@(ID _) , y ])      = printColonPair x y
printMaplikeElem (L [ x@(A (Lit _)) , y ]) = printColonPair x y
printMaplikeElem e                         = mkError "invalid map-like elem: " e


-- Tier 3 --

printColonPair :: E -> E -> Either Error Doc
printColonPair x y = do
    x' <- pprint x
    y' <- pprint y
    pure $ x' <> colon <+> y'

printIfThen :: E -> E -> Either Error Doc
printIfThen cond branch1 = do
    c' <- pprint cond
    b' <- pprint branch1
    pure $ text "if" <+> c' <+> bracedBlock b'

printAssignmentX :: Doc -> E -> Maybe E -> Maybe E -> Either Error Doc
printAssignmentX txt x t rhs = do
    x'   <- pprint x
    mt'  <- maybePrint id t
    rhs' <- maybePrint (equals <+>) rhs
    pure $ txt <+> x' <+> mt' <+> rhs'

printAssignmentXY :: Doc -> E -> E -> E -> Either Error Doc
printAssignmentXY txt x y rhs = do
    x'   <- pprint x
    y'   <- pprint y
    rhs' <- pprint rhs
    pure $ txt <+> x' <> comma <+> y' <+> equals <+> rhs'


-- Constants --

emptyBraces :: Doc
emptyBraces = text "{}"

kwConst :: Doc
kwConst = text "const"

kwVar :: Doc
kwVar = text "var"


-- Utils --

maybePrint :: (Doc -> Doc) -> Maybe E -> Either Error Doc
maybePrint f (Just x) = f <$> pprint x
maybePrint _ Nothing  = pure empty

doc :: Text -> Either Error Doc
doc = pure . textStrict

commaSep :: [Doc] -> Doc
commaSep xs = hsep (punctuate comma xs)

bracedBlock :: Doc -> Doc
bracedBlock x = braces ( line <> indent 2 x <> line )

mkError :: Text -> E -> Either Error Doc
mkError msg e = Left . TranslationError $ msg <> singleLine e
