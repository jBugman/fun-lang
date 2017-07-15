{-# LANGUAGE FlexibleContexts #-}
module Test.Utils where

import ClassyPrelude
import Test.Hspec                      (Expectation, HasCallStack, shouldBe, shouldSatisfy)
import Test.Hspec.Expectations.Contrib (isLeft)

import Fun             (SyntaxError)
import Fun.SExpression (Expression)


shouldParse :: (HasCallStack) => Either Text Expression -> Expression -> Expectation
actual `shouldParse` expected = actual `shouldBe` Right expected

shouldFailOn :: (HasCallStack) => (Text -> Either Text Expression) -> Text -> Expectation
parser `shouldFailOn` input = parser input `shouldSatisfy` isLeft

shouldPrint :: (HasCallStack) => Either SyntaxError Text -> Text -> Expectation
actual `shouldPrint` expected = actual `shouldBe` Right expected
