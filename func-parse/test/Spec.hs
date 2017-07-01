{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)

import Data.Aeson (toJSON, (.=))
import qualified Data.Aeson.Types as J (Value (Object, String, Array))

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

  describe "Fun.Parser.imports" $ do
    it "parses multiple imports" $
      prs imports "import \"pkg1\"\n\n\nimport \"pkg2\"\n" `shouldParse`
        [Import "pkg1" Nothing, Import "pkg2" Nothing]

    it "parses single import" $
      prs imports "import \"pkg1\"" `shouldParse` [Import "pkg1" Nothing]

    it "parses no imports" $
      prs imports "func main = undefined" `shouldParse` []

  describe "Fun.Parser.funcParams" $ do
    it "parses empty list" $
      prs funcParams "()" `shouldParse` []

    it "parses single parameter" $
      prs funcParams "(x bool)" `shouldParse` [Param $ VarSpec "x" (Atomic "bool")]

    it "parses multiple parameters" $
      prs funcParams "(n int, name string)" `shouldParse`
        [Param $ VarSpec "n" (Atomic "int"), Param $ VarSpec "name" (Atomic "string")]

  describe "Fun.Parser.funcResults" $ do
    it "parses empty list" $
      prs funcResults "" `shouldParse` []

    it "parses single result" $
      prs funcResults "bool" `shouldParse` [Atomic "bool"]

    it "parses multiple results" $
      prs funcResults "(int, error)" `shouldParse` [Atomic "int", Atomic "error"]

  describe "Fun.Parser.funFuncDecl" $ do
    it "parses simplest decl" $
      prs funFuncDecl "func f = undefined" `shouldParse` FuncDecl "f" [] [] Undefined

    it "parses func with some params" $
      prs funFuncDecl "func g (a int, b int) = undefined" `shouldParse`
        FuncDecl "g" [Param $ VarSpec "a" (Atomic "int"), Param $ VarSpec "b" (Atomic "int")] [] Undefined

    it "parses func witout params" $
      prs funFuncDecl "func read () -> (header, error) = undefined" `shouldParse`
        FuncDecl "read" [] [Atomic "header", Atomic "error"] Undefined

    it "parses params and results" $
      prs funFuncDecl "func h (a int, b string) -> (int, string) = undefined" `shouldParse`
        FuncDecl "h"
          [ Param $ VarSpec "a" (Atomic "int")
          , Param $ VarSpec "b" (Atomic "string")
          ] [ Atomic "int", Atomic "string" ] Undefined

    it "parses inline as body" $
      prs funFuncDecl "func test () = inline\n    ping <- true" `shouldParse`
        FuncDecl "test" [] [] (Inline ["ping <- true"])

  describe "Fun.Parser.inline" $ do
    it "fails on empty inline" $
      prs inline `shouldFailOn` "inline\n"

    it "parses inline func body" $
      prs inline "inline\n    i := 0\n    fmt.Printf(\"%d\", i)\n" `shouldParse`
        Inline ["i := 0", "fmt.Printf(\"%d\", i)"]

    it "parses inline func body with braces in it" $
      prs inline "inline\n    m := make(chan string)\n    go func() { {m <- struct{}{}} }()\n" `shouldParse`
        Inline ["m := make(chan string)", "go func() { {m <- struct{}{}} }()"]

    it "parses inline func body without something after it" $
      prs inline "inline\n    {\n        struct{}{}\n    }\n    // inline comment\n\n // comment" `shouldParse`
        Inline ["{", "    struct{}{}", "}", "// inline comment"]

  describe "Fun.Parser.funcNameP" $ do
    it "parses just name" $
      prs funcNameP "f" `shouldParse` FuncName "f"

    it "fails on not an ident" $
      prs funcNameP `shouldFailOn` "42"

    it "parses selector" $
      prs funcNameP "fmt.Println" `shouldParse` FuncName "fmt.Println"

  describe "Fun.Parser.literals" $ do
    describe "intLit" $ do
      it "parses unsigned int" $
        prs intLit "42" `shouldParse` IntegerLit 42

      it "parses signed int" $
        prs intLit "-42" `shouldParse` IntegerLit (-42)

      it "fails on string" $
        prs intLit `shouldFailOn` "foo"

    describe "doubleLit" $ do
      it "parses floats" $
        prs doubleLit "42.0" `shouldParse` DoubleLit 42.0

      it "fails on ints" $
        prs doubleLit `shouldFailOn` "42"

    describe "boolLit" $ do
      it "parses true" $
        prs boolLit "true" `shouldParse` BoolLit True

      it "parses false" $
        prs boolLit "false" `shouldParse` BoolLit False

      it "fails on string" $
        prs boolLit `shouldFailOn` "False"

    describe "hexLit" $ do
      it "fails on int" $
        prs hexLit `shouldFailOn` "42"

      it "parses hexes" $
        prs hexLit "0xFF" `shouldParse` HexLit 0xFF

    describe "stringLit" $ do
      it "parses string literal" $
        prs stringLit "\"foo\"" `shouldParse` StringLit "foo"

      it "parses quoted int as string" $
        prs stringLit "\"42\"" `shouldParse` StringLit "42"

      it "parses escaped chars" $
        prs stringLit "\"one\ntwo\"" `shouldParse` StringLit "one\ntwo"

    describe "charLit" $ do
      it "parses char literal" $
        prs charLit "'z'" `shouldParse` CharLit 'z'

      it "parses escaped char" $
        prs charLit "'\n'" `shouldParse` CharLit '\n'

      it "fails on broken char lit" $
        prs charLit `shouldFailOn` "'asd'"

      it "fails on string" $
        prs charLit `shouldFailOn` "\"42\""

  describe "Fun.Parser.expr" $ do
    it "parses int literal" $
      prs expr "42" `shouldParse` Lit (IntegerLit 42)

    it "parses string literal" $
      prs expr "\"foo\"" `shouldParse` Lit (StringLit "foo")

  describe "Fun.Parser.funcApplication" $ do
    it "parses simplest case" $
      prs funcApplication "f 42" `shouldParse` Application (FuncName "f") [Lit (IntegerLit 42)]

    it "parses println" $
      prs funcApplication "fmt.Println \"foo\"" `shouldParse`
        Application (FuncName "fmt.Println") [Lit (StringLit "foo")]

  describe "Fun.Parser.package" $ do
    it "parses helloworld" $
      prs package "package main\n\nfunc main = print \"hello world\"" `shouldParse`
        Package "main" [] [
            FuncDecl "main" [] []
              (Single $ Application (FuncName "print") [Lit $ StringLit "hello world"])]

    it "parses package with import" $
      prs package "package foo\n\nimport \"fmt\"\n\nfunc Hello = fmt.Println \"hello world\"" `shouldParse`
        Package "foo" [ Import "fmt" Nothing ] [
          FuncDecl "Hello" [] []
            (Single $ Application (FuncName "fmt.Println") [Lit $ StringLit "hello world"])]

    it "parses package with multiple imports" $
      prs package "package bar\n\nimport \"fmt\"\nimport \"looong/pkg\" as \"l\"\n\nfunc Greet = fmt.Println \"hello world\""
        `shouldParse`
        Package "bar"
          [ Import "fmt" Nothing
          , Import "looong/pkg" (Just "l")
          ][
          FuncDecl "Greet" [] []
            (Single $ Application (FuncName "fmt.Println") [Lit $ StringLit "hello world"])]

  describe "Fun.Types.ToJSON" $ do
    it "wrap works correctly" $ -- TODO use quickcheck
      wrap "Import" [ "path" .= ("fmt" :: String) ] `shouldBe`
        J.Object [ ("$type", J.String "Import"), ("$data", J.Object [ ("path", "fmt") ]) ]

    it "serializes helloworld" $
      toJSON
        (Package "main" [] [
          FuncDecl "main" [] []
            (Single $ Application (FuncName "print") [Lit $ StringLit "hello world"])])
        `shouldBe` wrap "Package"
          [ ("name", J.String "main")
          , ("imports", J.Array [])
          , ("topLevels", J.Array
            [ wrap "FuncDecl"
              [ ("name", J.String "main")
              , ("params", J.Array [])
              , ("results", J.Array [])
              , ("body", wrap "Single"
                [ ("expr", wrap "Application"
                  [ ("name", wrap "FuncName" [ ("v", J.String "print") ] )
                  , ("args", J.Array [ wrap "StringLit" [ ("value", J.String "hello world") ] ] )
                  -- Parens all the way down
                  ])
                ])
              ]
            ])
          ]
