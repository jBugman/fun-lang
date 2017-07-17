{-# LANGUAGE FlexibleContexts #-}
module Test.Utils
    ( shouldParse
    , shouldFailOn
    , shouldPrint
    , translationExample
) where

import ClassyPrelude
import System.FilePath                 ((<.>), (</>))
import Test.Hspec                      (Expectation, HasCallStack, Spec, describe, it, runIO,
                                        shouldBe, shouldSatisfy)
import Test.Hspec.Expectations.Contrib (isLeft)

import Fun             (translate)
import Fun.Errors      (Error, unError)
import Fun.SExpression (Expression)


-- API --

shouldParse :: (HasCallStack) => Either Error Expression -> Expression -> Expectation
actual `shouldParse` expected = actual `shouldBe` Right expected

shouldFailOn :: (HasCallStack) => (Text -> Either Error Expression) -> Text -> Expectation
parser `shouldFailOn` input = parser input `shouldSatisfy` isLeft

shouldPrint :: (HasCallStack) => Either Error Text -> Text -> Expectation
actual `shouldPrint` expected = actual `shouldBe` Right expected

translationExample :: (HasCallStack) => Text -> Spec
translationExample name = describe (unpack name) $ do
    (expected, actual) <- runIO (translateExample name)
    it "works" $ actual `shouldBe` expected


-- Tools --

translateExample :: Text -> IO (Text, Text)
translateExample name = do
    expected <- readGolden name
    actual   <- translated name
    return (expected, actual)

readSourceFile :: Text -> Text -> IO Text
readSourceFile name ext = readFileUtf8 ("examples" </> unpack name <.> unpack ext)

readGolden :: Text -> IO Text
readGolden path = readSourceFile path ".go"

translated :: Text -> IO Text
translated name = do
    src <- readSourceFile name ".fun"
    return $ either unError id $ translate src
