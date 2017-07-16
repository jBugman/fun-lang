{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
module Main where

import ClassyPrelude                hiding (print)
import Data.SCargot.Repr.WellFormed (pattern A, pattern L, pattern Nil)
import Test.Hspec                   (Spec, describe, it, shouldBe)
import Test.Tasty                   (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec             (testSpec)
import Test.Tasty.QuickCheck        (testProperty)

import Fun.Errors      (Error (..))
import Fun.Go.Desugar  (desugar)
import Fun.Go.Printer  (print, printPretty)
import Fun.Parser      (parse)
import Fun.SExpression (pattern BL, pattern CL, pattern DL, pattern HL, pattern ID, pattern IL,
                        pattern OP, pattern SL, pattern TP)
import Go.Fmt          (gofmt)
import Test.Examples   (examples)
import Test.Properties (exprFunctorCompose, exprFunctorIdentity)
import Test.Utils      (shouldFailOn, shouldParse, shouldPrint)


main :: IO ()
main = do
  hspUnit    <- testSpec "Unit"       unitsSpec
  hspFunc    <- testSpec "Functional" funcSpec
  defaultMain $ testGroup "Tests"
    [ hspUnit
    , hspFunc
    , testGroup "Properties" [ expressionFunctorProp ]
    , examples
    ]


expressionFunctorProp :: TestTree
expressionFunctorProp = testGroup "Expression Functor"
  [ testProperty "identity" exprFunctorIdentity
  , testProperty "composability" exprFunctorCompose
  ]


unitsSpec :: Spec
unitsSpec = do

  describe "Fun.Parser.parse" $ do

    it "fails on american double" $
      parse `shouldFailOn` ".223"

    it "fails on non-singleton char lit" $
      parse `shouldFailOn` "\'foo\'"

    it "fails on empty string" $
      parse `shouldFailOn` ""


    it "ignores comments A" $
      parse "; comment\nfoo" `shouldParse` ID "foo"

    it "ignores comments B" $
      parse "; comment 1\n(foo bar)\n; comment 2" `shouldParse` L [ ID "foo", ID "bar" ]


    it "empty list" $
      parse "()" `shouldParse` Nil

    it "string lit" $
      parse "\"test\"" `shouldParse` SL "test"

    it "char lit" $
      parse "\'z\'" `shouldParse` CL "z"

    it "char lit" $
      parse "'\\''" `shouldParse` CL "\\'"

    it "newline char" $
      parse "'\\n'" `shouldParse` CL "\\n"

    it "true" $
      parse "true" `shouldParse` BL True

    it "false" $
      parse "false" `shouldParse` BL False

    it "int lit" $
      parse "42" `shouldParse` IL 42

    it "zero" $
      parse "0" `shouldParse` IL 0

    it "double lit" $
      parse "42.0" `shouldParse` DL 42.0

    it "exp double lit" $
      parse "1e3" `shouldParse` DL 1000

    it "hex lit" $
      parse "0x2A" `shouldParse` HL 42

    it "ident" $
      parse "foo" `shouldParse` ID "foo"

    it "placeholder ident" $
      parse "_" `shouldParse` ID "_"

    it "Go selector" $
      parse "fmt.Println" `shouldParse` A "fmt.Println"

    it "type lit" $
      parse ":int" `shouldParse` TP "int"

    it "operator" $
      parse "+" `shouldParse` OP "+"

    it "&&" $
      parse "&&" `shouldParse` OP "&&"

    it "&" $
      parse "&" `shouldParse` OP "&"

    it "op + ident" $
      parse "(+ foo)" `shouldParse` L [ OP "+" , ID "foo" ]

    it "op hex int" $
      parse "(= 0xff 255)" `shouldParse` L [ OP "=" , HL 255 , IL 255 ]

    it "func call" $
      parse "(printf \"%+v\n\" v)" `shouldParse`
      L [ ID "printf", SL "%+v\n", ID "v" ]

    it "selector + unit" $
      parse "(fmt.Println ())" `shouldParse` L [ ID "fmt.Println", Nil ]

    it "ident list" $
      parse "(foo bar)" `shouldParse` L [ ID "foo", ID "bar" ]

    it "nested empty list" $
      parse "(())" `shouldParse` L [Nil]

    it "op + ident + lit" $
      parse "(< foo 10)" `shouldParse` L [ OP "<" , ID "foo" , IL 10 ]

    it "import" $
      parse "(import \"foo\")" `shouldParse` L [ ID "import" , SL "foo" ]

    it "multiline s-exp" $
      parse "(+ foo bar\n    :int)" `shouldParse`
      L [ OP "+" , ID "foo" , ID "bar" , TP "int" ]

    it "multiline s-exp with a comment" $
      parse "(foo 123 456\n; comment\n  bar)" `shouldParse`
      L [ ID "foo" , IL 123 , IL 456 , ID "bar" ]

    it "HelloWorld" $
      parse "(package main\n\n  (func main (print \"hello world\")))" `shouldParse`
      L [ ID "package" , ID "main"
        , L [ ID "func" , ID "main"
            , L [ ID "print" , SL "hello world" ] ]]


  describe "Fun.Go.Printer.print" $ do

    it "import" $
      print (L [ ID "import" , SL "fmt" ]) `shouldPrint` "import \"fmt\""

    it "import with alias" $
      print (L [ ID "import" , SL "very/long-package" , SL "pkg" ]) `shouldPrint`
      "import pkg \"very/long-package\""

    it "simple func" $
      print (L [ ID "func" , ID "setS" , L [ ID "set" , ID "s" , IL 2 ] ]) `shouldPrint`
      "func setS() {\ns = 2\n}"

    it "lt op" $
      print (L [ OP "<" , ID "n" , IL 10 ]) `shouldPrint` "n < 10"

    it "eq op" $
      print (L [ OP "=" , ID "foo" , ID "bar" ]) `shouldPrint` "foo == bar"

    it "const decl" $
      print (L [ ID "const" , ID "a" , SL "initial" ]) `shouldPrint` "const a = \"initial\""

    it "full var decl" $
      print (L [ ID "var" , ID "b", TP "int", IL 1 ]) `shouldPrint` "var b int = 1"

    it "infer bool var decl" $
      print (L [ ID "var" , ID "d", BL True ]) `shouldPrint` "var d = true"


funcSpec :: Spec
funcSpec = do

  describe "Fun.Go.Printer.printPretty" $ do

    it "import" $
      printPretty (L [ ID "import" , SL "fmt" ]) `shouldPrint` "import \"fmt\""

    it "import with alias" $ printPretty ( L
      [ ID "import" , SL "very/long-package" , SL "pkg" ]) `shouldPrint`
      "import pkg \"very/long-package\""

    it "simple func" $ printPretty ( L
      [ ID "func" , ID "setS" , L [ ID "set", ID "s", IL 2 ] ]) `shouldPrint`
      "func setS() {\n\ts = 2\n}"

    it "lt op" $
      printPretty (L [ OP "<" , ID "n" , IL 10 ]) `shouldPrint` "n < 10"

    it "eq op" $
      printPretty (L [ OP "=" , ID "foo" , ID "bar" ]) `shouldPrint` "foo == bar"


  describe "Go.Fmt.gofmt" $ do

    it "formats valid code" $
      gofmt "func  foo  (  ) { \n i++}" `shouldBe` Right "func foo() {\n\ti++\n}"  -- TODO: shouldParse, change Left

    it "returns err on a broken code" $
      gofmt "func foo }( __" `shouldBe` Left (GoError "1:20: expected '(', found '}'")


  describe "Fun.Go.Desugar.desugar" $ do
    it "does nothing when there is nothing to do" $
      desugar (L [ ID "foo" , ID "bar" ]) `shouldBe` L [ ID "foo" , ID "bar" ]

    it "desugars print" $
      desugar (L
        [ ID "package" , ID "main" , L
        [ ID "func" , ID "main" , L
          [ ID "print" , SL "hello world"] ]])
      `shouldBe` L
        [ ID "package", ID "main" , L
        [ ID "import" , SL "fmt" ] , L
        [ ID "func" , ID "main" , L
          [ ID "fmt.Println" , SL "hello world"] ]]

    it "desugars print with existing import" $
      desugar (L
        [ ID "package" , ID "main" , L
        [ ID "import" , SL "fmt" ] , L
        [ ID "func" , ID "main" , L
          [ ID "print" , SL "hello world"] ]])
      `shouldBe` L
        [ ID "package" , ID "main" , L
        [ ID "import" , SL "fmt" ] , L
        [ ID "func" , ID "main" , L
          [ ID "fmt.Println" , SL "hello world"] ]]
