{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Instances (
    Arbitrary (..)
) where

import ClassyPrelude            hiding (pack, unpack)
import Data.Text                (pack, unpack)
import Test.QuickCheck
import Test.QuickCheck.Function

import qualified Fun.SExpression as S


-- Expression --

genBasic :: Gen S.Expression
genBasic = frequency
    [ (1, return S.Unit)
    , (2, return (S.Atom "foo"))
    , (2, return (S.Atom "bar"))
    , (2, return (S.Atom "baz"))
    , (2, return (S.Type ":string"))
    , (3, return (S.Op "+")) ]

genElems :: Gen [S.Expression]
genElems = resize 10 $ listOf1 genBasic

genExp :: Gen S.Expression
genExp = S.Exp <$> genElems

genList :: Gen S.Expression
genList = S.List <$> genElems

genExpression :: Gen S.Expression
genExpression = frequency
    [ (3, genBasic)
    , (1, genList)
    , (4, genExp) ]

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
