{-# LANGUAGE FlexibleContexts #-}
module Test.Utils
    ( shouldPrint
    , translationExample
    , i
    , int
    , op
    , str
    , tp
) where

import ClassyPrelude
import System.FilePath ((<.>), (</>))
import Test.Hspec      (Expectation, HasCallStack, Spec, describe, it, runIO, shouldBe)

import Fun             (translateFmt)
import Fun.Errors      (Error, unError)
import Fun.SExpression (Atom (..), Expression (..), Literal (..))


-- Patterns --

i :: Text -> Expression
i x = Atom (Ident x) Nothing

int :: Integer -> Expression
int x = Atom (Literal (Integer 10 x)) Nothing

op :: Text -> Expression
op x = Atom (Operator x) Nothing

str :: Text -> Expression
str x = Atom (Literal (String x)) Nothing

tp :: Text -> Expression
tp x = Atom (Type x) Nothing

-- API --

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
