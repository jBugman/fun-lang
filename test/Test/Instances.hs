{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Instances (
    Arbitrary (..)
) where

import ClassyPrelude
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Fun.Sexp as S


genExpr :: Gen (S.Expression Text)
genExpr = do
    t <- arbitrary
    oneof [return S.Unit, return (S.Atom t)] -- TODO: Gen for Exp and List

instance Arbitrary (S.Expression Text) where
    arbitrary = genExpr
