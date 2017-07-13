{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Instances (
    Arbitrary (..)
) where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed (WellFormedSExpr (..))
import Test.QuickCheck              (Arbitrary, CoArbitrary, Gen, arbitrary, coarbitrary, frequency,
                                     listOf1, oneof, resize)
import Test.QuickCheck.Function     (Function, function, functionMap)

import Fun.SExpression (Atom (..), Expression)


-- Atomic --

genIdent :: Gen Atom
genIdent = frequency
    [ (2, return (Ident "foo"))
    , (2, return (Ident "bar"))
    , (2, return (Ident "baz")) ]

genType :: Gen Atom
genType = frequency
    [ (1, return (Type ":string"))
    , (1, return (Type ":int")) ]

genOp :: Gen Atom
genOp = return (Op "+")

genAtom :: Gen Atom
genAtom = frequency
    [ (3, genIdent)
    , (2, genType)
    , (1, genOp) ]

instance Arbitrary Atom where
    arbitrary = genAtom

instance Function Atom where
    function = functionMap show fromString

instance CoArbitrary Atom where
    coarbitrary = coarbitrary . show

-- Expression --

genAtomic :: Gen Expression
genAtomic = WFSAtom <$> genAtom

genElem :: Gen Expression
genElem = frequency
    [ (1, return (WFSList []))
    , (5, genAtomic) ]

genList :: Gen Expression
genList = WFSList <$> resize 10 (listOf1 genElem)

genExpression :: Gen Expression
genExpression = frequency
    [ (3, genAtomic)
    , (4, genList) ]

instance Arbitrary Expression where
    arbitrary = genExpression


-- Text --

genText :: Gen Text
genText = pack <$> resize 8 (listOf1 genLetter)
    where
        genLetter :: Gen Char
        genLetter = oneof $ fmap return letters
        letters :: [Char]
        letters = ['A'..'Z'] <> ['0'..'9']

instance Arbitrary Text where
    arbitrary = genText

instance Function Text where
    function = functionMap unpack pack

instance CoArbitrary Text where
    coarbitrary = coarbitrary . unpack
