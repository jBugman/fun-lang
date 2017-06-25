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

  describe "Fun.Parser.funcResults" $ do
    it "parses empty list" $
      p funcResults "" `shouldBe` Right []

    it "parses single result" $
      p funcResults "bool" `shouldBe` Right [Type "bool"]

    it "parses multiple results" $
      p funcResults "(int, error)" `shouldBe` Right [Type "int", Type "error"]

  describe "Fun.Parser.funFuncDecl" $ do
    it "parses simplest decl" $
      p funFuncDecl "func f = undefined" `shouldBe` Right (FuncDecl "f" [] [] Undefined)

    it "parses func with some params" $
      p funFuncDecl "func g (a :: int, b :: int) = undefined" `shouldBe`
        Right (FuncDecl "g" [Param "a" (Type "int"), Param "b" (Type "int")] [] Undefined)

    it "parses func witout params" $
      p funFuncDecl "func read () -> (header, error) = undefined" `shouldBe`
        Right (FuncDecl "read" [] [Type "header", Type "error"] Undefined)

    it "parses params and results" $
      p funFuncDecl "func h (a :: int, b :: string) -> (int, string) = undefined" `shouldBe`
        Right (FuncDecl "h" [Param "a" (Type "int"), Param "b" (Type "string")] [Type "int", Type "string"] Undefined)

  -- describe "Fun.Parser.inline" $ do
  --   it "parses empty inline" $
  --     p inline "inline {}" `shouldBe` Right (Inline "")

  --   it "parses inline func body" $
  --     p inline "inline {\n    i := 0\n    fmt.Printf(\"%d\", i)\n}" `shouldBe`
  --       Right (Inline "\n    i := 0\n    fmt.Printf(\"%d\", i)\n")

  --   it "parses inline func body with braces in it" $
  --     p inline "inline {\n    m := make(chan string)\n    go func() { {m <- struct{}{}} }()\n}" `shouldBe`
  --       Right (Inline "\n    m := make(chan string)\n    go func() { {m <- struct{}{}} }()\n")

  --   it "parses inline func body without something after it" $
  --     p inline "inline {\n  {\n struct{}{}\n }\n }\n}\n // comment" `shouldBe`
  --       Right (Inline "\n  {\n struct{}{}\n }\n }\n")
