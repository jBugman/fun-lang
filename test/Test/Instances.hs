{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Instances (
    Arbitrary (..)
) where

import ClassyPrelude                hiding (pack, unpack)
import Data.SCargot.Repr.WellFormed
import Data.Text                    (pack, unpack)
import Test.QuickCheck
import Test.QuickCheck.Function

import qualified Fun.SExpression as S


-- Atomic --

genIdent :: Gen S.Atom
genIdent = frequency
    [ (2, return (S.Ident "foo"))
    , (2, return (S.Ident "bar"))
    , (2, return (S.Ident "baz")) ]

genType :: Gen S.Atom
genType = frequency
    [ (1, return (S.Type ":string"))
    , (1, return (S.Type ":int")) ]

genOp :: Gen S.Atom
genOp = return (S.Op "+")

genAtom :: Gen S.Atom
genAtom = frequency
    [ (3, genIdent)
    , (2, genType)
    , (1, genOp) ]

instance Arbitrary S.Atom where
    arbitrary = genAtom

instance Function S.Atom where
    function = functionMap show fromString

instance CoArbitrary S.Atom where
    coarbitrary = coarbitrary . show

-- Expression --

genAtomic :: Gen S.Expression
genAtomic = WFSAtom <$> genAtom

genElem :: Gen S.Expression
genElem = frequency
    [ (1, return (WFSList []))
    , (5, genAtomic) ]

genList :: Gen S.Expression
genList = WFSList <$> resize 10 (listOf1 genElem)

genExpression :: Gen S.Expression
genExpression = frequency
    [ (3, genAtomic)
    , (4, genList) ]

instance Arbitrary S.Expression where
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
