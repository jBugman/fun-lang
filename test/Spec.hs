module Main where

import ClassyPrelude
import Test.Hspec            (describe, hspec, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)

import Fun.Go.Desugar
import Fun.Go.Printer
import Fun.Main       (translate')
import Fun.Parser
import Go.Fmt

import qualified Fun.Sexp as S


main :: IO ()
main = hspec $ do

  describe "S.Expression is a MonoFunctor" $
    it "omaps on S.Exp" $
      omap toUpper (S.Exp ["foo", S.Unit, "42", S.Exp ["barbar"]])
        `shouldBe` S.Exp [S.Atom "FOO", S.Unit, S.Atom "42", S.Exp[S.Atom "BARBAR" :: S.Expression Text]]


  describe "Fun.Parser.sunit" $ do
    it "parses SUnit" $
      prs sunit "()" `shouldParse` S.Unit

    it "fails on random input" $
      prs sunit `shouldFailOn` "foo"


  describe "Fun.Parser.satom" $ do
    it "parses string lit" $
      prs satom "\"test\"" `shouldParse` S.Atom "\"test\""

    it "parses int lit" $
      prs satom "42" `shouldParse` S.Atom "42"

    it "parses ident" $
      prs satom "foo" `shouldParse` S.Atom "foo"

    it "ignores comments" $
      prs satom "; comment\nfoo" `shouldParse` S.Atom "foo"

    it "parses Go selector" $
      prs satom "fmt.Println" `shouldParse` S.Atom "fmt.Println"

    it "fails on unit" $
      prs satom `shouldFailOn` "()"

    it "fails on type lit" $
      prs satom `shouldFailOn` ":int"


  describe "Fun.Parser.sop" $
    it "parses operator" $
      prs sop "+" `shouldParse` S.Op "+"


  describe "Fun.Parser.stype" $
    it "parses type lit" $
      prs stype ":int" `shouldParse` S.Type "int"


  describe "Fun.Parser.list" $ do
    it "parses empty string" $
      prs list "" `shouldParse` []

    it "parses op + ident" $
      prs list "+ foo" `shouldParse` [S.Op "+", S.Atom "foo"]

    it "parses func call" $
      prs list "printf \"%+v\n\" v" `shouldParse` [S.Atom "printf", S.Atom "\"%+v\n\"", S.Atom "v"]

    it "parses selecor + unit" $
      prs list "fmt.Println ()" `shouldParse` [S.Atom "fmt.Println", S.Unit]


  describe "Fun.Parser.slist" $ do
    it "fails on empty string" $
      prs slist `shouldFailOn` ""

    it "parses ident list" $
      prs slist "[foo bar]" `shouldParse` S.List [S.Atom "foo", S.Atom "bar"]

    it "ignores comments" $
      prs slist "; comment 1\n[foo bar]\n; comment 2" `shouldParse` S.List ["foo", "bar"]


  describe "Fun.Parser.stuple" $ do
    it "parses unit" $
      prs stuple "(())" `shouldParse` S.Exp [S.Unit]

    it "parses op + ident + lit" $
      prs stuple "(< foo 10)" `shouldParse` S.Exp [S.Op "<", S.Atom "foo", S.Atom "10"]

    it "ignores comments" $
      prs stuple "; comment\n(foo 10)" `shouldParse` S.Exp ["foo", "10"]

    it "parses func call" $
      prs stuple "(printf \"%+v\n\" v)" `shouldParse` S.Exp [S.Atom "printf", S.Atom "\"%+v\n\"", S.Atom "v"]

    it "parses selecor + unit" $
      prs stuple "(fmt.Println ())" `shouldParse` S.Exp [S.Atom "fmt.Println", S.Unit]


  describe "Fun.Parser.sexp" $ do
    it "parses ident" $
      prs sexp "foo" `shouldParse` S.Atom "foo"

    it "ignores comments" $
      prs sexp "; this is a comment\nfoo" `shouldParse` S.Atom "foo"

    it "parses comparison" $
      prs sexp "(< foo 10)" `shouldParse` S.Exp [S.Op "<", S.Atom "foo", S.Atom "10"]

    it "parses import" $
      prs sexp "(import \"foo\")" `shouldParse` S.Exp ["import", "\"foo\""]

    it "parses multiline s-exp" $
      prs sexp "(foo 123 456\n    789)" `shouldParse` S.Exp ["foo", "123", "456", "789"]

    it "parses multiline s-exp with a comment" $
      prs sexp "(foo 123 456\n; comment\n  bar)" `shouldParse` S.Exp ["foo", "123", "456", "bar"]

    it "parses HelloWorld" $
      prs sexp "(package main\n\n  (func main (print \"hello world\")))" `shouldParse` S.Exp
          [ S.Atom "package", S.Atom "main", S.Exp
            [ S.Atom "func", S.Atom "main", S.Exp
              [ S.Atom "print", S.Atom "\"hello world\""]]]


  describe "Fun.Go.Printer.printPretty" $ do
    it "prints import" $
      printPretty (S.Exp ["import", "\"fmt\""]) `shouldBe` Right "import \"fmt\""

    it "prints import with alias" $
      printPretty (S.Exp ["import", "\"very/long-package\"", "\"pkg\""])
        `shouldBe` Right "import pkg \"very/long-package\""

    it "prints simple func" $
      printPretty (S.Exp ["func", "setS", S.Exp ["set", "s", "2"]]) `shouldBe` Right
        "func setS() {\n\ts = 2\n}"

    it "prints lt op" $
      printPretty (S.Exp ["<", "n", "10"]) `shouldBe` Right "n < 10"

    it "prints eq op" $
      printPretty (S.Exp ["=", "foo", "bar"]) `shouldBe` Right "foo == bar"


  describe "Go.Fmt.gofmt" $ do
    it "formats valid code" $
      gofmt "func  foo  (  ) { \n i++}" `shouldBe` Right "func foo() {\n\ti++\n}"

    it "returns err on a broken code" $
      gofmt "func foo }( __" `shouldBe` Left "1:20: expected '(', found '}'"


  describe "Fun.Go.Desugar.desugar" $ do
    it "does nothing when there is nothing to do" $
      desugar (S.Exp ["foo", "bar"]) `shouldBe` S.Exp ["foo", "bar"]

    it "desugars print" $
      desugar (S.Exp ["package", "main", S.Exp ["func", "main", S.Exp ["print", "\"hello world\""]]])
        `shouldBe` S.Exp
          ["package", "main"
          , S.Exp ["import", "\"fmt\""]
          , S.Exp ["func", "main", S.Exp ["fmt.Println", "\"hello world\""]]]

    it "desugars print with existing import" $
      desugar (S.Exp
        ["package", "main"
        , S.Exp ["import", "\"fmt\""]
        , S.Exp ["func", "main", S.Exp ["print", "\"hello world\""]]])
          `shouldBe` S.Exp
          ["package", "main"
          , S.Exp ["import", "\"fmt\""]
          , S.Exp ["func", "main", S.Exp ["fmt.Println", "\"hello world\""]]]


  describe "Fun.Main.translate" $
    it "works on example 01" $
      translate' "(package main\n\n(func main (print \"hello world\")))\n" `shouldBe`
        Right "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"hello world\")\n}\n"
