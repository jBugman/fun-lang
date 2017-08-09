{-# LANGUAGE FlexibleContexts #-}
module Test.Utils
    ( shouldParse
    , shouldFailOn
    , shouldPrint
    , translationExample
    , pos
    , pos1
    , op
    , str
    , int
) where

import ClassyPrelude
import System.FilePath                 ((<.>), (</>))
import Test.Hspec                      (Expectation, HasCallStack, Spec, describe, it, runIO,
                                        shouldBe, shouldSatisfy)
import Test.Hspec.Expectations.Contrib (isLeft)

import Fun             (translateFmt)
import Fun.Errors      (Error, Pos (..), unError)
import Fun.SExpression (Atom (..), Expression (..), Literal (..))


-- Patterns --

str :: Text -> Expression
str x = Atom (Literal (String x)) Nothing

op :: Text -> Expression
op x = Atom (Operator x) Nothing

int :: Integer -> Expression
int x = Atom (Literal (Integer 10 x)) Nothing

pos1 :: Maybe Pos
pos1 = pos 1 1

pos :: Int -> Int -> Maybe Pos
pos l c = Just (Pos l c)

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
    return . either unError id $ translateFmt src
