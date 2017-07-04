{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (print)
import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)

import Fun.Parser
import qualified Fun.Sexp as S
import Fun.GoPrinter (print)


main :: IO ()
main = hspec $ do
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

    it "parses operator" $
      prs satom "+" `shouldParse` S.Atom "+"

    it "parses ident" $
      prs satom "foo" `shouldParse` S.Atom "foo"

    it "parses Go selector" $
      prs satom "fmt.Println" `shouldParse` S.Atom "fmt.Println"

    it "fails on unit" $
      prs satom `shouldFailOn` "()"

  describe "Fun.Parser.list" $ do
    it "parses empty string" $
      prs list "" `shouldParse` []

    it "parses op + ident" $
      prs list "+ foo" `shouldParse` [S.Atom "+", S.Atom "foo"]

    it "parses func call" $
      prs list "printf \"%+v\n\" v" `shouldParse` [S.Atom "printf", S.Atom "\"%+v\n\"", S.Atom "v"]

    it "parses selecor + unit" $
      prs list "fmt.Println ()" `shouldParse` [S.Atom "fmt.Println", S.Unit]

  describe "Fun.Parser.slist" $ do
    it "fails on empty string" $
      prs slist `shouldFailOn` ""

    it "parses ident list" $
      prs slist "[foo bar]" `shouldParse` S.List [S.Atom "foo", S.Atom "bar"]

  describe "Fun.Parser.stuple" $ do
    it "parses unit" $
      prs stuple "(())" `shouldParse` S.Exp [S.Unit]

    it "parses op + ident + lit" $
      prs stuple "(< foo 10)" `shouldParse` S.Exp [S.Atom "<", S.Atom "foo", S.Atom "10"]

    it "parses func call" $
      prs stuple "(printf \"%+v\n\" v)" `shouldParse` S.Exp [S.Atom "printf", S.Atom "\"%+v\n\"", S.Atom "v"]

    it "parses selecor + unit" $
      prs stuple "(fmt.Println ())" `shouldParse` S.Exp [S.Atom "fmt.Println", S.Unit]

  describe "Fun.Parser.sexp" $ do
    it "parses ident" $
      prs sexp "foo" `shouldParse` S.Atom "foo"

    it "parses comparison" $
      prs sexp "(< foo 10)" `shouldParse` S.Exp [S.Atom "<", S.Atom "foo", S.Atom "10"]

    it "parses import" $
      prs sexp "import \"foo\"" `shouldParse` S.Exp ["import", "\"foo\""]

    it "parses HelloWorld" $
      prs sexp "(package main\n\n  (func main (print \"hello world\")))" `shouldParse` S.Exp
          [ S.Atom "package", S.Atom "main", S.Exp
            [ S.Atom "func", S.Atom "main", S.Exp
              [ S.Atom "print", S.Atom "\"hello world\""]]]


  describe "Fun.GoPrinter.print" $ do
    it "prints import" $
      print (S.Exp ["import", "fmt"]) `shouldBe` Right "import \"fmt\""

    it "prints import with alias" $
      print (S.Exp ["import", "very/long-package", "pkg"]) `shouldBe` Right "import pkg \"very/long-package\""
