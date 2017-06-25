import Test.Hspec

import Fun.Parser
import Fun.Types


main :: IO ()
main = hspec $ do
  describe "Fun.Parser.funImport" $ do
    it "parses short form" $
      prs funImport "import \"fmt\"" `shouldBe` Right (Import "fmt" Nothing)

    it "returns error on malformed input" $
      show (prs funImport "i_mport \"fmt\"") `shouldBe` "Left (line 1, column 1):\nunexpected \"_\"\nexpecting \"import\""

    it "parses alias" $
      prs funImport "import \"longpackagename\" as \"pkg\"" `shouldBe` Right (Import "longpackagename" (Just "pkg"))

    it "parses nested packages" $
      prs funImport "import \"io/ioutil\"" `shouldBe` Right (Import "io/ioutil" Nothing)

    it "parses github urls" $
      prs funImport "import \"github.com/jBugman/fun-lang/fun\"" `shouldBe` Right (Import "github.com/jBugman/fun-lang/fun" Nothing)

  describe "Fun.Parser.funcParams" $ do
    it "parses empty list" $
      prs funcParams "()" `shouldBe` Right []

    it "parses single parameter" $
      prs funcParams "(x :: bool)" `shouldBe` Right [Param "x" (Type "bool")]

    it "parses multiple parameters" $
      prs funcParams "(n :: int, name :: string)" `shouldBe` Right [Param "n" (Type "int"), Param "name" (Type "string")]

  describe "Fun.Parser.funcResults" $ do
    it "parses empty list" $
      prs funcResults "" `shouldBe` Right []

    it "parses single result" $
      prs funcResults "bool" `shouldBe` Right [Type "bool"]

    it "parses multiple results" $
      prs funcResults "(int, error)" `shouldBe` Right [Type "int", Type "error"]

  describe "Fun.Parser.funFuncDecl" $ do
    it "parses simplest decl" $
      prs funFuncDecl "func f = undefined" `shouldBe` Right (FuncDecl "f" [] [] Undefined)

    it "parses func with some params" $
      prs funFuncDecl "func g (a :: int, b :: int) = undefined" `shouldBe`
        Right (FuncDecl "g" [Param "a" (Type "int"), Param "b" (Type "int")] [] Undefined)

    it "parses func witout params" $
      prs funFuncDecl "func read () -> (header, error) = undefined" `shouldBe`
        Right (FuncDecl "read" [] [Type "header", Type "error"] Undefined)

    it "parses params and results" $
      prs funFuncDecl "func h (a :: int, b :: string) -> (int, string) = undefined" `shouldBe`
        Right (FuncDecl "h" [Param "a" (Type "int"), Param "b" (Type "string")] [Type "int", Type "string"] Undefined)

  -- describe "Fun.Parser.inline" $ do
  --   it "parses empty inline" $
  --     prs inline "inline {}" `shouldBe` Right (Inline "")

  --   it "parses inline func body" $
  --     prs inline "inline {\n    i := 0\n    fmt.Printf(\"%d\", i)\n}" `shouldBe`
  --       Right (Inline "\n    i := 0\n    fmt.Printf(\"%d\", i)\n")

  --   it "parses inline func body with braces in it" $
  --     prs inline "inline {\n    m := make(chan string)\n    go func() { {m <- struct{}{}} }()\n}" `shouldBe`
  --       Right (Inline "\n    m := make(chan string)\n    go func() { {m <- struct{}{}} }()\n")

  --   it "parses inline func body without something after it" $
  --     prs inline "inline {\n  {\n struct{}{}\n }\n }\n}\n // comment" `shouldBe`
  --       Right (Inline "\n  {\n struct{}{}\n }\n }\n")
