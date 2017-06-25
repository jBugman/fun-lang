import Test.Hspec (hspec, describe, it)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)

import Fun.Parser
import Fun.Types


main :: IO ()
main = hspec $ do
  describe "Fun.Parser.funImport" $ do
    it "parses short form" $
      prs funImport "import \"fmt\"" `shouldParse` Import "fmt" Nothing

    it "returns error on malformed input" $
      prs funImport `shouldFailOn` "i_mport \"fmt\""

    it "checks what import must not be indented" $
      prs funImport `shouldFailOn` "    import \"fmt\""

    it "parses alias" $
      prs funImport "import \"longpackagename\" as \"pkg\"" `shouldParse` Import "longpackagename" (Just "pkg")

    it "parses nested packages" $
      prs funImport "import \"io/ioutil\"" `shouldParse` Import "io/ioutil" Nothing

    it "parses github urls" $
      prs funImport "import \"github.com/jBugman/fun-lang/fun\"" `shouldParse` Import "github.com/jBugman/fun-lang/fun" Nothing

  describe "Fun.Parser.funcParams" $ do
    it "parses empty list" $
      prs funcParams "()" `shouldParse` []

    it "parses single parameter" $
      prs funcParams "(x :: bool)" `shouldParse` [Param "x" (Type "bool")]

    it "parses multiple parameters" $
      prs funcParams "(n :: int, name :: string)" `shouldParse` [Param "n" (Type "int"), Param "name" (Type "string")]

  describe "Fun.Parser.funcResults" $ do
    it "parses empty list" $
      prs funcResults "" `shouldParse` []

    it "parses single result" $
      prs funcResults "bool" `shouldParse` [Type "bool"]

    it "parses multiple results" $
      prs funcResults "(int, error)" `shouldParse` [Type "int", Type "error"]

  describe "Fun.Parser.funFuncDecl" $ do
    it "parses simplest decl" $
      prs funFuncDecl "func f = undefined" `shouldParse` FuncDecl "f" [] [] Undefined

    it "parses func with some params" $
      prs funFuncDecl "func g (a :: int, b :: int) = undefined" `shouldParse`
        FuncDecl "g" [Param "a" (Type "int"), Param "b" (Type "int")] [] Undefined

    it "parses func witout params" $
      prs funFuncDecl "func read () -> (header, error) = undefined" `shouldParse`
        FuncDecl "read" [] [Type "header", Type "error"] Undefined

    it "parses params and results" $
      prs funFuncDecl "func h (a :: int, b :: string) -> (int, string) = undefined" `shouldParse`
        FuncDecl "h" [Param "a" (Type "int"), Param "b" (Type "string")] [Type "int", Type "string"] Undefined

  describe "Fun.Parser.inline" $ do
    it "parses empty inline" $
      prs inline "inline\n" `shouldParse` Inline []

    it "parses inline func body" $
      prs inline "inline\n    i := 0\n    fmt.Printf(\"%d\", i)\n" `shouldParse`
        Inline ["i := 0", "fmt.Printf(\"%d\", i)"]

    it "parses inline func body with braces in it" $
      prs inline "inline\n    m := make(chan string)\n    go func() { {m <- struct{}{}} }()\n" `shouldParse`
        Inline ["m := make(chan string)", "go func() { {m <- struct{}{}} }()"]

    it "parses inline func body without something after it" $
      prs inline "inline\n    {\n        struct{}{}\n    }\n    // inline comment\n\n // comment" `shouldParse`
        Inline ["{", "    struct{}{}", "}", "// inline comment"]
