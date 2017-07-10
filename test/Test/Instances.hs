{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Instances (
    Arbitrary (..)
) where

import ClassyPrelude
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Fun.Sexp as S


genBasic :: Gen (S.Expression Text)
genBasic = do
    t <- arbitrary
    frequency [ (1, return S.Unit)
              , (4, return (S.Atom t)) ]

genElems :: Gen [S.Expression Text]
genElems = resize 10 $ listOf1 genBasic

genExp :: Gen (S.Expression Text)
genExp = S.Exp <$> genElems

genList :: Gen (S.Expression Text)
genList = S.List <$> genElems

genExpression :: Gen (S.Expression Text)
genExpression = frequency
    [ (3, genBasic)
    , (1, genList)
    , (4, genExp) ]

instance Arbitrary (S.Expression Text) where
    arbitrary = genExpression
