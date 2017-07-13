{-# LANGUAGE PatternSynonyms #-}
module Main where

import ClassyPrelude                hiding (print)
import Data.SCargot.Repr.WellFormed (pattern A, pattern L, pattern Nil)
import Test.Hspec                   (Expectation, describe, expectationFailure, hspec, it, shouldBe)

-- import           Fun.Go.Desugar
-- import           Fun.Main        (translate')
import Fun.Go.Printer  (print, printPretty)
import Fun.Parser      (parse)
import Fun.SExpression (Expression, pattern ID, pattern IL, pattern OP, pattern SL, pattern TP)
import Go.Fmt
import Test.Properties



shouldParse :: Either Text Expression -> Expression -> Expectation
r `shouldParse` v = case r of
    Left e  -> expectationFailure . unpack $ "expected: " <> tshow v <> "\nbut parsing failed with error:\n" <> e
    Right x -> unless (x == v) . expectationFailure $ "expected: " <> show v <> "\nbut got: " <> show x

shouldFailOn :: (Text -> Either Text Expression) -> Text -> Expectation
p `shouldFailOn` s = case p s of
  Left _  -> return ()
  Right v -> expectationFailure . unpack $ "the parser is expected to fail, but it parsed: " <> show v


main :: IO ()
main = hspec $ do

  describe "Expression Functor" $ do
    -- it "manual fmap" $
    --   fmap toUpper (S.List ["foo", S.List [], "42", S.List ["barbar"]])
    --     `shouldBe` S.List [S.Atom (S.Ident "FOO"), S.List [], S.Atom (S.Ident "42"), S.List [S.Atom (S.Ident "BARBAR")]]

    it "identity" exprFunctorIdentity

    it "composability" exprFunctorCompose


  describe "Fun.Parser.parse" $ do

    it "parses empty list" $
      parse "()" `shouldParse` Nil

    it "parses string lit" $
      parse "\"test\"" `shouldParse` SL "test"

    it "parses int lit" $
      parse "42" `shouldParse` IL 42

    it "parses ident" $
      parse "foo" `shouldParse` ID "foo"

    it "ignores comments" $
      parse "; comment\nfoo" `shouldParse` ID "foo"

    it "parses Go selector" $
      parse "fmt.Println" `shouldParse` A "fmt.Println"

    it "parses type lit" $
      parse ":int" `shouldParse` TP ":int"

    it "parses operator" $
      parse "+" `shouldParse` OP "+"

    it "fails on empty string" $
      parse `shouldFailOn` ""

    it "parses op + ident" $
      parse "(+ foo)" `shouldParse` L [ OP "+" , ID "foo" ]

    it "parses func call" $
      parse "(printf \"%+v\n\" v)" `shouldParse`
      L [ ID "printf", SL "\"%+v\n\"", ID "v" ]

    it "parses selecor + unit" $
      parse "(fmt.Println ())" `shouldParse` L [ ID "fmt.Println", Nil ]

    it "parses ident list" $
      parse "(foo bar)" `shouldParse` L [ ID "foo", ID "bar" ]

    it "ignores comments" $
      parse "; comment 1\n(foo bar)\n; comment 2" `shouldParse` L [ ID "foo", ID "bar" ]

    it "parses nested empty list" $
      parse "(())" `shouldParse` L [Nil]

    it "parses op + ident + lit" $
      parse "(< foo 10)" `shouldParse` L [ OP "<" , ID "foo" , IL 10 ]

    it "parses import" $
      parse "(import \"foo\")" `shouldParse` L [ ID "import" , SL "\"foo\"" ]

    it "parses multiline s-exp" $
      parse "(+ foo bar\n    :int)" `shouldParse`
      L [ OP "+" , ID "foo" , ID "bar" , TP ":int" ]

    it "parses multiline s-exp with a comment" $
      parse "(foo 123 456\n; comment\n  bar)" `shouldParse`
      L [ ID "foo" , IL 123 , IL 456 , ID "bar" ]

    it "parses HelloWorld" $
      parse "(package main\n\n  (func main (print \"hello world\")))" `shouldParse`
      L [ ID "package" , ID "main"
        , L [ ID "func" , ID "main"
            , L [ ID "print" , SL "\"hello world\"" ]
            ]
        ]


  describe "Fun.Go.Printer.print" $ do

    it "prints import" $
      print (L [ ID "import" , SL "\"fmt\"" ]) `shouldBe`
      Right "import \"fmt\""

    it "prints import with alias" $
      print (L [ ID "import" , SL "\"very/long-package\"" , SL "\"pkg\"" ]) `shouldBe`
      Right "import pkg \"very/long-package\""

    it "prints simple func" $
      print (L [ ID "func" , ID "setS" , L [ ID "set" , ID "s" , IL 2 ] ]) `shouldBe`
      Right "func setS() {\ns = 2\n}"

    it "prints lt op" $
      print (L [ OP "<" , ID "n" , IL 10 ]) `shouldBe` Right "n < 10"

    it "prints eq op" $
      print (L [ OP "=" , ID "foo" , ID "bar" ]) `shouldBe` Right "foo == bar"


  describe "Fun.Go.Printer.printPretty" $ do

    it "prettyprints import" $
      printPretty (L [ ID "import" , SL "\"fmt\"" ]) `shouldBe`
      Right "import \"fmt\""

    it "prettyprints import with alias" $ printPretty (L
      [ ID "import" , SL "\"very/long-package\"" , SL "\"pkg\"" ]) `shouldBe`
      Right "import pkg \"very/long-package\""

    it "prettyprints simple func" $ printPretty (L
      [ ID "func" , ID "setS" , L [ ID "set", ID "s", IL 2 ] ]) `shouldBe`
      Right "func setS() {\n\ts = 2\n}"

    it "prettyprints lt op" $
      printPretty (L [ OP "<" , ID "n" , IL 10 ]) `shouldBe` Right "n < 10"

    it "prettyprints eq op" $
      printPretty (L [ OP "=" , ID "foo" , ID "bar" ]) `shouldBe` Right "foo == bar"


  describe "Go.Fmt.gofmt" $ do

    it "formats valid code" $
      gofmt "func  foo  (  ) { \n i++}" `shouldBe` Right "func foo() {\n\ti++\n}"

    it "returns err on a broken code" $
      gofmt "func foo }( __" `shouldBe` Left "1:20: expected '(', found '}'"


  -- describe "Fun.Go.Desugar.desugar" $ do
  --   it "does nothing when there is nothing to do" $
  --     desugar (S.Exp ["foo", "bar"]) `shouldBe` S.Exp ["foo", "bar"]

  --   it "desugars print" $
  --     desugar (S.Exp ["package", "main", S.Exp ["func", "main", S.Exp ["print", "\"hello world\""]]])
  --       `shouldBe` S.Exp
  --         ["package", "main"
  --         , S.Exp ["import", "\"fmt\""]
  --         , S.Exp ["func", "main", S.Exp ["fmt.Println", "\"hello world\""]]]

  --   it "desugars print with existing import" $
  --     desugar (S.Exp
  --       ["package", "main"
  --       , S.Exp ["import", "\"fmt\""]
  --       , S.Exp ["func", "main", S.Exp ["print", "\"hello world\""]]])
  --         `shouldBe` S.Exp
  --         ["package", "main"
  --         , S.Exp ["import", "\"fmt\""]
  --         , S.Exp ["func", "main", S.Exp ["fmt.Println", "\"hello world\""]]]


  -- describe "Fun.Main.translate" $
  --   it "works on example 01" $
  --     translate' "(package main\n\n(func main (print \"hello world\")))\n" `shouldBe`
  --       Right "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"hello world\")\n}\n"

