module Main where

import ClassyPrelude
import Data.SCargot.Repr.WellFormed
import Test.Hspec                   (Expectation, describe, expectationFailure, hspec, it, shouldBe)

-- import           Fun.Go.Desugar
-- import           Fun.Go.Printer
-- import           Fun.Main        (translate')
import Fun.Parser      (parse)
import Fun.SExpression (Atom (..), Expression)
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
      parse "\"test\"" `shouldParse` A "\"test\"" -- FIXME:

    it "parses int lit" $
      parse "42" `shouldParse` A "42" -- FIXME:

    it "parses ident" $
      parse "foo" `shouldParse` A (Ident "foo")

    it "ignores comments" $
      parse "; comment\nfoo" `shouldParse` A (Ident "foo")

    it "parses Go selector" $
      parse "fmt.Println" `shouldParse` A "fmt.Println"

    it "parses type lit" $
      parse ":int" `shouldParse` A (Type ":int")

    it "parses operator" $
      parse "+" `shouldParse` A (Op "+")

    it "fails on empty string" $
      parse `shouldFailOn` ""

    it "parses op + ident" $
      parse "(+ foo)" `shouldParse` L [A (Op "+"), A (Ident "foo")]

    it "parses func call" $
      parse "(printf \"%+v\n\" v)" `shouldParse` L [A (Ident "printf"), A "\"%+v\n\"", A (Ident "v")] -- FIXME:

    it "parses selecor + unit" $
      parse "(fmt.Println ())" `shouldParse` L [A "fmt.Println", Nil] -- FIXME:

    it "parses ident list" $
      parse "(foo bar)" `shouldParse` L [A (Ident "foo"), A (Ident "bar")]

    it "ignores comments" $
      parse "; comment 1\n(foo bar)\n; comment 2" `shouldParse` L [A (Ident "foo"), A (Ident "bar")]

    it "parses nested empty list" $
      parse "(())" `shouldParse` L [Nil]

    it "parses op + ident + lit" $
      parse "(< foo 10)" `shouldParse` L [A (Op "<"), A (Ident "foo"), A "10"] -- FIXME:

    it "parses import" $
      parse "(import \"foo\")" `shouldParse` L [A "import", A "\"foo\""] -- FIXME:

    it "parses multiline s-exp" $
      parse "(+ foo bar\n    :int)" `shouldParse`
      L [A (Op "+"), A (Ident "foo"), A (Ident "bar"), A (Type ":int")]

    it "parses multiline s-exp with a comment" $
      parse "(foo 123 456\n; comment\n  bar)" `shouldParse`
      L [A (Ident "foo"), A "123", A "456", A (Ident "bar")]

    it "parses HelloWorld" $
      parse "(package main\n\n  (func main (print \"hello world\")))" `shouldParse`
      L [ A (Ident "package"), A (Ident "main")
        , L [ A (Ident "func"), A (Ident "main")
            , L [ A (Ident "print"), A "\"hello world\"" ] -- FIXME:
            ]
        ]


  -- describe "Fun.Go.Printer.printPretty" $ do
  --   it "prints import" $
  --     printPretty (S.Exp ["import", "\"fmt\""]) `shouldBe` Right "import \"fmt\""

  --   it "prints import with alias" $
  --     printPretty (S.Exp ["import", "\"very/long-package\"", "\"pkg\""])
  --       `shouldBe` Right "import pkg \"very/long-package\""

  --   it "prints simple func" $
  --     printPretty (S.Exp ["func", "setS", S.Exp ["set", "s", "2"]]) `shouldBe` Right
  --       "func setS() {\n\ts = 2\n}"

  --   it "prints lt op" $
  --     printPretty (S.Exp ["<", "n", "10"]) `shouldBe` Right "n < 10"

  --   it "prints eq op" $
  --     printPretty (S.Exp ["=", "foo", "bar"]) `shouldBe` Right "foo == bar"


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

