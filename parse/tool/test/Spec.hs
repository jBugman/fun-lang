{-# LANGUAGE FlexibleContexts #-}
import Test.Hspec

import Data.Functor.Identity (Identity)
import Text.Parsec (parse, Parsec, Stream, ParseError)

import Fun.Parser
import Fun.Types

p :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
p rule = parse rule ""

main :: IO ()
main = hspec $ do
  describe "Fun.Parser.funImport" $ do
    it "parses short form" $
      p funImport "import \"fmt\"" `shouldBe` Right (Import "fmt" Nothing)

    it "returns error on malformed input" $
      show (p funImport "i_mport \"fmt\"") `shouldBe` "Left (line 1, column 1):\nunexpected \"_\"\nexpecting \"import\""

    it "parses alias" $
      p funImport "import \"longpackagename\" as \"pkg\"" `shouldBe` Right (Import "longpackagename" (Just "pkg"))

    it "parses nested packages" $
      p funImport "import \"io/ioutil\"" `shouldBe` Right (Import "io/ioutil" Nothing)

    it "parses github urls" $
      p funImport "import \"github.com/jBugman/fun-lang/fun\"" `shouldBe` Right (Import "github.com/jBugman/fun-lang/fun" Nothing)

  describe "Fun.Parser.funcParams" $ do
    it "parses empty list" $
      p funcParams "()" `shouldBe` Right []

    it "parses single parameter" $
      p funcParams "(x :: bool)" `shouldBe` Right [Param "x" (Type "bool")]

    it "parses multiple parameters" $
      p funcParams "(n :: int, name :: string)" `shouldBe` Right [Param "n" (Type "int"), Param "name" (Type "string")]

-- describe "Fun.Parser.funFuncDecl" $ do
--   it "parses simplest decl" $
--     p funFuncDecl "func f\n" `shouldBe` Right (Fun.FuncDecl "f" [] Fun.JustIO [] Undefined)

--   it "parses some params" $
--     p funFuncDecl "func g (a :: int, b :: int)\n" `shouldBe`
--       Right (FuncDecl "g" [Param "a" "int", Param "b" "int"] [] Undefined)

--   it "parses return tuple" $
--     p funFuncDecl "func h (a :: int, b :: string) -> (int, string)\n" `shouldBe`
--       Right (FuncDecl "f" [Param "a" "int", Param "b" "string"] [Type "int", Type "string"] Undefined)
