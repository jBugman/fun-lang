{-# LANGUAGE FlexibleContexts #-}
module Test.Utils where

import ClassyPrelude
import Test.Hspec                      (Expectation, HasCallStack, shouldBe, shouldSatisfy)
import Test.Hspec.Expectations.Contrib (isLeft)

import Fun.Errors      (Error)
import Fun.SExpression (Expression)


shouldParse :: (HasCallStack) => Either Error Expression -> Expression -> Expectation
actual `shouldParse` expected = actual `shouldBe` Right expected

shouldFailOn :: (HasCallStack) => (Text -> Either Error Expression) -> Text -> Expectation
parser `shouldFailOn` input = parser input `shouldSatisfy` isLeft

shouldPrint :: (HasCallStack) => Either Error Text -> Text -> Expectation
actual `shouldPrint` expected = actual `shouldBe` Right expected
