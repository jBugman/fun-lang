{-# LANGUAGE PatternSynonyms #-}
module Fun.Go.Printer ( printGo ) where

import ClassyPrelude                hiding (empty)
import Data.Either.Extra            (maybeToEither)
import Text.PrettyPrint.Leijen.Text (Doc, braces, brackets, colon, comma, displayTStrict, dot,
                                     empty, equals, hcat, hsep, indent, line, parens, pretty,
                                     punctuate, renderPretty, semi, space, text, textStrict, vsep,
                                     (<+>))

import Fun.Errors        (Error (..))
import Fun.PrettyPrinter (singleLine)
import Fun.SExpression   (Atom (..), pattern DL, Expression (..), pattern I, pattern ID, pattern KW,
                          pattern L, pattern LIT, pattern Nil, pattern OP, pattern SL, pattern TP,
                          unPos)

type E = Expression

printGo :: E -> Either Error Text
printGo x = displayTStrict . renderPretty 0.6 100 <$> pprint x


pprint :: E -> Either Error Doc
-- ident
pprint (ID x _) = doc x

-- operator
pprint (OP x _) = doc x

-- literal
pprint (LIT x _) = pure $ pretty x

-- types
pprint (TP "any" _)  = doc "interface{}"
pprint (TP x _)      = doc x

pprint (L [ TP "ptr" _ , x ]) = do
    x' <- pprint x
    pure $ text "*" <> x'

pprint (L [ TP "chan" _ , x ]) = do
    x' <- pprint x
    pure $ text "chan" <+> x'

pprint (L [ TP "slice" _ , x ]) = do
    x' <- pprint x
    pure $ text "[]" <> x'

pprint (L [ TP "map" _ , k@(TP _ _) , v ]) = do
    k' <- pprint k
    v' <- pprint v
    pure $ text "map" <> brackets k' <> v'

pprint (L [ TP "func" _ , a ]) = do
    a' <- printArgs a
    pure $ kwFunc <> a'

pprint (L [ TP "func" _ , a , r ]) = do
    a' <- printArgs a
    r' <- pprint r
    pure $ kwFunc <> a' <+> r'

-- type alias
pprint (L [ KW "alias" , n@(ID _ _) , t ]) = do
    n' <- pprint n
    t' <- pprint t
    pure $ kwType <+> n' <+> t'

-- const
pprint (L [ KW "const" , x@(ID _ _) , rhs ])
    = printAssignmentX kwConst x Nothing (Just rhs)

pprint (L [ KW "const" , x@(ID _ _) , t@(TP _ _) , rhs ])
    = printAssignmentX kwConst x (Just t) (Just rhs)

-- var
pprint (L [ KW "var" , x@(ID _ _) , t@(TP _ _) ])
    = printAssignmentX kwVar x (Just t) Nothing

pprint (L [ KW "var" , x@(ID _ _) , rhs@(L [ TP _ _ , L _ ]) ])
    = printAssignmentX kwVar x Nothing (Just rhs)

pprint (L [ KW "var" , x@(ID _ _) , rhs@(L [ L( TP _ _ : _ ) , L _ ]) ])
    = printAssignmentX kwVar x Nothing (Just rhs)

pprint (L [ KW "var" , x@(ID _ _) , t@( L( TP _ _ : _ )) ])
    = printAssignmentX kwVar x (Just t) Nothing

pprint (L [ KW "var" , x@(ID _ _) , y@(ID _ _) , rhs ])
    = (kwVar <+>) <$> printAssignmentXY x (Just y) rhs

pprint (L [ KW "var" , x@(ID _ _) , t@(TP _ _) , rhs ])
    = printAssignmentX kwVar x (Just t) (Just rhs)

pprint (L [ KW "var" , x@(ID _ _) , rhs ])
    = printAssignmentX kwVar x Nothing (Just rhs)

-- struct decl
pprint (L [ KW "struct" , n@(ID _ _) ]) = do
    n' <- pprint n
    pure $ kwType <+> n' <+> text "struct" <> emptyBraces

pprint (L ( KW "struct" : n@(ID _ _) : xs )) = do
    n'  <- pprint n
    xs' <- mapM printStructElem xs
    pure $ kwType <+> n' <+> text "struct"
        <+> bracedBlock (vsep xs')

-- interface decl
pprint (L [ KW "interface" , n@(ID _ _) ]) = do
    n'  <- pprint n
    pure $ kwType <+> n' <+> text "interface" <> emptyBraces

pprint (L ( KW "interface" : n@(ID _ _) : xs )) = do
    n'  <- pprint n
    xs' <- mapM printInterfaceElem xs
    pure $ kwType <+> n' <+> text "interface"
        <+> bracedBlock (vsep xs')

-- func
pprint (L [ KW "func" , n@(ID _ _) , b ])
    = printFunc Nothing (Just n) Nothing Nothing b

pprint (L [ KW "func" , n@(ID _ _) , a , b ])
    = printFunc Nothing (Just n) (Just a) Nothing b

pprint (L [ KW "func" , n@(ID _ _) , a , r , b ])
    = printFunc Nothing (Just n) (Just a) (Just r) b

-- method
pprint (L [ KW "method" , o , n@(ID _ _) , b ])
    = printFunc (Just o) (Just n) Nothing Nothing b

pprint (L [ KW "method" , o , n@(ID _ _) , a , b ])
    = printFunc (Just o) (Just n) (Just a) Nothing b

pprint (L [ KW "method" , o , n@(ID _ _) , a , r , b ])
    = printFunc (Just o) (Just n) (Just a) (Just r) b

-- unary operators
pprint (L [ op@(OP _ _) , x ]) = do
    op' <- pprint op
    x'  <- pprint x
    pure $ case op of
        OP "++" _ -> x'  <> op'
        OP "--" _ -> x'  <> op'
        _         -> op' <> x'

-- binary operators
pprint (L [ OP "==" _ , lhs , rhs ]) = do
    lhs' <- pprint lhs
    rhs' <- pprint rhs
    pure $ lhs' <+> text "==" <+> rhs'

pprint (L ( OP "." _ : xs )) = do -- chained function call
    xs' <- mapM pprint xs
    pure $ hcat (punctuate dot xs')

pprint (L ( op@(OP _ _) : xs )) = do
    op' <- pprint op
    xs' <- mapM pprint xs
    pure $ hsep (intersperse op' xs')

-- assignment
pprint (L [ KW "set" , x , rhs ])
    = printAssignmentXY x Nothing rhs

pprint (L [ KW "set" , x@(ID _ _) , y@(ID _ _) , rhs ])
    = printAssignmentXY x (Just y) rhs

-- import
pprint (L [ KW "import" , x@(SL _ _) ]) = do
    x' <- pprint x
    pure $ text "import" <+> x'

pprint (L [ KW "import" , x@(SL _ _) , SL alias _ ]) = do
    x' <- pprint x
    pure $ text "import" <+> textStrict alias <+> x'

-- return
pprint (L ( KW "return" : xs ) ) = do
    xs' <- mapM pprint xs
    pure $ text "return" <+> commaSep xs'

-- range
pprint (L [ KW "range" , x@(ID _ _) , xs ]) = do
    x'  <- pprint x
    xs' <- pprint xs
    pure $ x' <+> text ":=" <+> text "range" <+> xs'

pprint (L [ KW "range" , x@(ID _ _) , y@(ID _ _) , xs ]) = do
    x'  <- pprint x
    y'  <- pprint y
    xs' <- pprint xs
    pure $ x' <> comma <+> y' <+> text ":=" <+> text "range" <+> xs'

-- indexed access
pprint (L [ KW "val" , n@(ID _ _) , i ]) = do
    n' <- pprint n
    i' <- pprint i
    pure $ n' <> brackets i'

pprint (L [ KW "val" , n@(ID _ _) , i , j ]) = do
    n' <- pprint n
    i' <- pprint i
    j' <- pprint j
    pure $ n' <> brackets i' <> brackets j'

-- slice literal
pprint (L [ t@(L [ TP "slice" _ , _ ]) , L xs ]) = do
    t'  <- pprint t
    xs' <- mapM pprint xs
    pure $ t' <> braces (commaSep xs')

-- map literal
pprint (L [ t@(L [ TP "map" _ , _ , _ ]) , L xs ])
    = printMapLike t xs

-- struct literal
pprint (L [ t@(TP _ _) ])        = printMapLike t []
pprint (L [ t@(TP _ _) , L xs ]) = printMapLike t xs

-- complex literal
pprint (L [ TP "complex" _ , x@(I _ _) , y@(I _ _) ])
    = printComplex x y
pprint (L [ TP "complex" _ , x@(I _ _) , y@(DL _ _) ])
    = printComplex x y
pprint (L [ TP "complex" _ , x@(DL _ _) , y@(I _ _) ])
    = printComplex x y
pprint (L [ TP "complex" _ , x@(DL _ _) , y@(DL _ _) ])
    = printComplex x y

-- function call
pprint (L [ f@(ID _ _) ])
    = printCallLike f Nothing

pprint (L ( f@(ID _ _) : a )) = do
    f' <- pprint f
    a' <- mapM pprint a
    pure $ f' <> parens (commaSep a')

-- make
pprint (L [ KW "make" , s@(L ( TP "slice" _ : _ )) , d ]) = do
    s' <- pprint s
    d' <- pprint d
    pure $ text "make" <> parens (s'<> comma <+> d')

pprint (L [ KW "make" , x@(L [ TP "map" _ , _ , _ ]) ]) = do
    x' <- pprint x
    pure $ text "make" <> parens x'

pprint (L [ KW "make" , x@(L [TP "chan" _ , _ ]) ]) = do
    x' <- pprint x
    pure $ text "make" <> parens x'

-- type casting
pprint (L [ KW "cast" , t@(TP _ _) , x ])
    = printCallLike t (Just x)

pprint (L [ KW "cast" , t@(L (TP _ _ : _ )) , x ])
    = printCallLike t (Just x)

-- type assertion
pprint (L [ KW "assert" , t@(TP _ _) , x ])
    = printAssert t x

-- lambda
pprint (L [ KW "func" , Nil , b ])
    = printFunc Nothing Nothing Nothing Nothing b

pprint (L [ KW "func" , a , b ])
    = printFunc Nothing Nothing (Just a) Nothing b

pprint (L [ KW "func" , a , r , b ])
    = printFunc Nothing Nothing (Just a) (Just r) b

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

-- switch
pprint (L [ KW "switch" , L xs ]) = do
    xs' <- mapM printCase xs
    pure $ text "switch" <+> bracedBlock (vsep xs')

pprint (L [ KW "switch" , L [ KW "type" , t@(ID _ _) , x ] , L xs ]) = do
    t'  <- pprint t
    x'  <- printAssert (TP "type" Nothing) x
    xs' <- mapM printCase xs
    pure $ text "switch"
        <+> t' <+> text ":=" <+> x'
        <+> bracedBlock (vsep xs')

pprint (L [ KW "switch" , x , L xs ]) = do
    x'  <- pprint x
    xs' <- mapM printCase xs
    pure $ text "switch" <+> x' <+> bracedBlock (vsep xs')

-- for loop
pprint (L [ KW "for" , body ]) = do
    body' <- pprint body
    pure $ text "for" <+> bracedBlock body'

pprint (L [ KW "for" , cond , body ]) = do
    cond' <- pprint cond
    body' <- pprint body
    pure $ text "for" <+> cond' <+> bracedBlock body'

pprint (L [ KW "for" , i@(ID _ _) , from , cond , iter , body ])
    = printFor i from cond iter body

pprint (L [ KW "for" , i@(ID _ _) , from , to , body ])
    = printFor
        i
        from
        (L [ Atom (Operator "<") Nothing  , i , to ])
        (L [ Atom (Operator "++") Nothing , i ])
        body

-- package
pprint (L ( KW "package" : n@(ID _ _) : topLevels )) = do
    n'  <- pprint n
    ts' <- mapM pprint topLevels
    pure . vsep . punctuate line $ ((text "package" <+> n') : ts')

-- slicing
pprint (L [ KW "slice" , x@(ID _ _) , ID "_" _ , to ]) = do
    x'  <- pprint x
    to' <- pprint to
    pure $ x' <> brackets ( colon <> to' )

pprint (L [ KW "slice" , x@(ID _ _) , from , ID "_" _ ]) = do
    x'    <- pprint x
    from' <- pprint from
    pure $ x' <> brackets ( from' <> colon )

pprint (L [ KW "slice" , x@(ID _ _) , from , to ]) = do
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
pprint x = mkError "not supported yet: " x


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
printRecv t@(TP _ _) = do
    t' <- pprint t
    pure $ parens t'
printRecv (L [ n@(ID _ _) , t ]) = do
    n' <- pprint n
    t' <- pprint t
    pure $ parens (n' <+> t')
printRecv e = mkError "invalid receiver: " e

printArgs :: E -> Either Error Doc
printArgs Nil    = pure $ parens empty
printArgs (L xs) = parens . commaSep <$> mapM printArg xs
printArgs e      = mkError "invalid args: " e

printArg :: E -> Either Error Doc
printArg t@(TP _ _)             = pprint t
printArg t@(L (TP _ _ : _))     = pprint t
printArg (L [ n@(ID _ _) , t ]) = do
    n' <- pprint n
    t' <- pprint t
    pure $ n' <+> t'
printArg e                    = mkError "invalid arg: " e

printResults :: E -> Either Error Doc
printResults t@(TP _ _) = pprint t
printResults t@(L ( TP "func" _ : _ )) = pprint t
printResults (L ts)  = do
    ts' <- mapM pprint ts
    pure $ parens (commaSep ts')
printResults e        = mkError "invalid results: " e

printBody :: E -> Either Error Doc
printBody Nil = pure emptyBraces
printBody xs  = bracedBlock <$> pprint xs

printStructElem :: E -> Either Error Doc
printStructElem x@(TP _ _)
    = pprint x
printStructElem x@(L [ TP "ptr" _ , TP _ _ ])
    = pprint x
printStructElem (L [ x@(ID _ _) , y ]) = do
    x' <- pprint x
    y' <- pprint y
    pure $ x' <+> y'
printStructElem e = mkError "invalid struct elem: " e

printInterfaceElem :: E -> Either Error Doc
printInterfaceElem x@(TP _ _)
    = pprint x
printInterfaceElem (L [ n@(ID _ _) , a ]) = do
    n' <- pprint n
    a' <- printArgs a
    pure $ n' <> a'
printInterfaceElem (L [ n@(ID _ _) , a , r ]) = do
    n' <- pprint n
    a' <- printArgs a
    r' <- printResults r
    pure $ n' <> a' <+> r'
printInterfaceElem e = mkError "invalid interface elem: " e

printMapLike :: E -> [E] ->  Either Error Doc
printMapLike t xs = do
    t'  <- pprint t
    xs' <- mapM printMaplikeElem xs
    pure $ t' <> braces (commaSep xs')

printMaplikeElem :: E -> Either Error Doc
printMaplikeElem (L [ x@(ID _ _) , y ])  = printColonPair x y
printMaplikeElem (L [ x@(LIT _ _) , y ]) = printColonPair x y
printMaplikeElem e                       = mkError "invalid map-like elem: " e

printFunc :: Maybe E -> Maybe E -> Maybe E -> Maybe E -> E -> Either Error Doc
printFunc recv name args res body = do
    r'  <- maybeWith (space <>) empty printRecv recv
    n'  <- maybeWith (space <>) empty pprint name
    a'  <- maybe' (parens empty) printArgs args
    rs' <- maybe' empty printResults res
    b'  <- printBody body
    pure $ kwFunc <> r' <> n' <> a' <+> rs' <+> b'

printCallLike :: E -> Maybe E -> Either Error Doc
printCallLike f x = do
    f' <- pprint f
    x' <- maybe' empty pprint x
    pure $ f' <> parens x'

printAssignmentX :: Doc -> E -> Maybe E -> Maybe E -> Either Error Doc
printAssignmentX txt x t rhs = do
    x'   <- pprint x
    t'   <- maybe' empty pprint t
    rhs' <- maybeWith (equals <+>) empty pprint rhs
    pure $ txt <+> x' <+> t' <+> rhs'

printAssignmentXY :: E -> Maybe E -> E -> Either Error Doc
printAssignmentXY x y rhs = do
    x'   <- pprint x
    y'   <- maybeWith (\t -> comma <+> t <> space) space pprint  y
    rhs' <- pprint rhs
    pure $ x' <> y' <> equals <+> rhs'

printCase :: E -> Either Error Doc
printCase (L [ KW "default" , xs ]) = do
    xs' <- pprint xs
    pure $ text "default" <> colon <+> xs'
printCase (L [ KW "case" , x ]) = do
    x' <- pprint x
    pure $ text "case" <+> x' <> colon
printCase (L ( KW "case" : xxs )) = do
    (cs, xs) <- maybeToEither
        (mkError' "invalid case: " (L xxs))
        $ unsnoc xxs
    cs' <- mapM pprint cs
    xs' <- pprint xs
    pure $ text "case" <+> commaSep cs' <> colon <+> xs'
printCase e = mkError "invalid case clause: " e

printAssert :: E -> E -> Either Error Doc
printAssert t x = do
    t' <- pprint t
    x' <- pprint x
    pure $ x' <> dot <> parens t'

printComplex :: E -> E -> Either Error Doc
printComplex x y = do
    x' <- pprint x
    y' <- pprint y
    let sign = bool (text "+") empty ("-" `isPrefixOf` tshow y')
    pure $ x' <> sign <> y' <> text "i"


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


-- Constants --

emptyBraces :: Doc
emptyBraces = text "{}"

kwConst :: Doc
kwConst = text "const"

kwVar :: Doc
kwVar = text "var"

kwType :: Doc
kwType = text "type"

kwFunc :: Doc
kwFunc = text "func"


-- Utils --

maybeWith :: (Doc -> Doc)-> Doc -> (E -> Either Error Doc) -> Maybe E -> Either Error Doc
maybeWith g z f = maybe (pure z) (fmap g . f)

maybe' :: Doc -> (E -> Either Error Doc) -> Maybe E -> Either Error Doc
maybe' z = maybe (pure z)

doc :: Text -> Either Error Doc
doc = pure . textStrict

commaSep :: [Doc] -> Doc
commaSep xs = hsep (punctuate comma xs)

bracedBlock :: Doc -> Doc
bracedBlock x = braces ( line <> indent 2 x <> line )

mkError :: Text -> E -> Either Error Doc
mkError msg e = Left $ mkError' msg e

mkError' :: Text -> E -> Error
mkError' msg e = TranslationError (unPos e) (msg <> singleLine e)
