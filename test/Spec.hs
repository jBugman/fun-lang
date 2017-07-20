{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
module Main where

import ClassyPrelude                hiding (print)
import Data.Either.Combinators      (mapLeft)
import Data.SCargot.Repr.WellFormed (pattern L, pattern Nil)
import Test.Hspec                   (Spec, describe, hspec, it, shouldBe)

import Fun.Desugar       (desugar)
import Fun.Errors        (Error (..), unError)
import Fun.Go.Printer    (print)
import Fun.Parser        (parse)
import Fun.PrettyPrinter (singleLine)
import Fun.SExpression   (pattern BL, pattern CL, pattern DL, pattern HL, pattern ID, pattern IL,
                          pattern KW, pattern OP, pattern SL, pattern TP)
import Go.Fmt            (gofmt)
import Test.Utils        (shouldFailOn, shouldParse, shouldPrint, translationExample)


main :: IO ()
main = hspec $ describe "Everything" $ do
  dummyTests
  describe "Tests" $ do
    unitTests
    integrationTests
  examples


examples :: Spec
examples = describe "Examples" $ do
  translationExample "01_hello_world"
  translationExample "02_values"
  translationExample "03_variables"
  translationExample "04_for"
  translationExample "05_if_else"
  -- translationExample "06_switch"  -- TODO: fix nested function call spec
  translationExample "07_slices"
  translationExample "08_maps"
  translationExample "09_range"
  translationExample "10_functions"
  translationExample "11_closures"
  translationExample "12_pointers"


unitTests :: Spec
unitTests = do

  describe "Fun.Parser.parse" $ do

    it "fails on american double" $
      parse `shouldFailOn` ".223"

    it "fails on non-singleton char lit" $
      parse `shouldFailOn` "\'foo\'"

    it "fails on empty string" $
      parse `shouldFailOn` ""

    it "fails on garbage imput" $
      mapLeft unError (parse "_BANG!!") `shouldBe`
      Left "syntax error: (line 1, column 2):\nunexpected 'B'\nexpecting space, comment or end of input"


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
      parse "fmt.Println" `shouldParse` ID "fmt.Println"

    it "type lit" $
      parse ":int" `shouldParse` TP "int"

    it "+" $
      parse "+" `shouldParse` OP "+"

    it "++" $
      parse "++" `shouldParse` OP "++"

    it "&&" $
      parse "&&" `shouldParse` OP "&&"

    it "&" $
      parse "&" `shouldParse` OP "&"

    it "op + ident" $
      parse "(+ foo)" `shouldParse` L [ OP "+" , ID "foo" ]

    it "op hex int" $
      parse "(= 0xff 255)" `shouldParse` L [ OP "=" , HL 255 , IL 255 ]

    it "keyword" $
      parse "for" `shouldParse` KW "for"

    it "not a keyword" $
      parse "forall" `shouldParse` ID "forall"

    it "func call" $
      parse "(printf \"%+v\n\" v)" `shouldParse`
      L [ KW "printf", SL "%+v\n", ID "v" ]

    it "selector + unit" $
      parse "(fmt.Println ())" `shouldParse` L [ ID "fmt.Println", Nil ]

    it "ident list" $
      parse "(foo bar)" `shouldParse` L [ ID "foo", ID "bar" ]

    it "nested empty list" $
      parse "(())" `shouldParse` L [Nil]

    it "op + ident + lit" $
      parse "(< foo 10)" `shouldParse` L [ OP "<" , ID "foo" , IL 10 ]

    it "import" $
      parse "(import \"foo\")" `shouldParse` L [ KW "import" , SL "foo" ]

    it "multiline s-exp" $
      parse "(+ foo bar\n    :int)" `shouldParse`
      L [ OP "+" , ID "foo" , ID "bar" , TP "int" ]

    it "multiline s-exp with a comment" $
      parse "(foo 123 456\n; comment\n  bar)" `shouldParse`
      L [ ID "foo" , IL 123 , IL 456 , ID "bar" ]

    it "HelloWorld" $
      parse "(package main\n\n  (func main (print \"hello world\")))" `shouldParse`
      L [ KW "package" , ID "main"
        , L [ KW "func" , ID "main"
            , L [ KW "print" , SL "hello world" ] ]]

    it "var decl from slice literal" $
      parse "(var t (:slice :string) (\"g\" \"h\" \"c\"))" `shouldParse`
      L [ KW "var" , ID "t" , L [ TP "slice" , TP "string" ] , L [ SL "g" , SL "h" , SL "c" ] ]


  describe "Fun.Go.Printer.print" $ do

    it "fail on standalone Nil" $
      mapLeft unError (print Nil) `shouldBe` Left "translation error: empty expression"

    it "import" $
      print (L [ KW "import" , SL "fmt" ]) `shouldPrint` "import \"fmt\""

    it "import with alias" $
      print (L [ KW "import" , SL "very/long-package" , SL "pkg" ]) `shouldPrint`
      "import pkg \"very/long-package\""

    it "simple func" $
      print (L [ KW "func" , ID "setS" , L [ KW "set" , ID "s" , IL 2 ] ]) `shouldPrint`
      "func setS() {\ns = 2\n}"

    it "<" $
      print (L [ OP "<" , ID "n" , IL 10 ]) `shouldPrint` "n < 10"

    it "==" $
      print (L [ OP "=" , ID "foo" , ID "bar" ]) `shouldPrint` "foo == bar"

    it "--" $
      print (L [ OP "--" , ID "j" ]) `shouldPrint` "j--"

    it "hex == char" $
      print (L [ OP "=" , HL 1 , CL "a" ]) `shouldPrint` "0x1 == 'a'"

    it "double" $
      print (DL 9.99) `shouldPrint` "9.99"

    it "string" $
      print (SL "fizzbuzz") `shouldPrint` "\"fizzbuzz\""

    it "'any' type" $
      print (TP "any") `shouldPrint` "interface{}"

    it "const decl" $
      print (L [ KW "const" , ID "a" , SL "initial" ]) `shouldPrint` "const a = \"initial\""

    it "full const decl" $
      print (L [ KW "const" , ID "a" , TP "State" , HL 10 ]) `shouldPrint` "const a State = 0xa"

    it "full var decl" $
      print (L [ KW "var" , ID "b" , TP "int" , IL 1 ]) `shouldPrint` "var b int = 1"

    it "infer type var" $
      print (L [ KW "var" , ID "d" , BL True ]) `shouldPrint` "var d = true"

    it "zero value var" $
      print (L [ KW "var" , ID "x" , TP "int" ]) `shouldPrint` "var x int"

    it "zero value var with composite type" $
      print (L [ KW "var" , ID "x" , L [ TP "slice" , TP "string" ] ]) `shouldPrint` "var x []string"

    it "var with slice literal initializer" $
      print (L [ KW "var" , ID "nums" , L [ TP "slice" , TP "int" ] , L [ IL 1 , IL 2, IL 3 ] ])
      `shouldPrint`
      "var nums = []int{1, 2, 3}"

    it "var with map literal initializer" $
      print (L [ KW "var" , ID "m"
      , L [ TP "map" , TP "string" , TP "bool" ]
      , L [ L [ SL "foo", BL True ] , L [ SL "bar" , BL False ] ] ]) `shouldPrint`
      "var m = map[string]bool{\"foo\": true, \"bar\": false}"

    it "slice of slices" $
      print (L [ TP "slice" , L [ TP "slice" , TP "string" ] ]) `shouldPrint` "[][]string"

    it "map lookup" $
      print (L [ KW "set" , ID "_" , ID "ok" , L [ KW "val" , ID "m" , SL "Bob" ] ])
      `shouldPrint`
      "_, ok = m[\"Bob\"]"

    it "slice index" $
      print (L [ KW "val" , ID "xs" , IL 4 ]) `shouldPrint` "xs[4]"

    it "slice 2D-index" $
      print (L [ KW "val" , ID "xs" , ID "y" , ID "x" ]) `shouldPrint` "xs[y][x]"

    it "short range" $
      print (L [ KW "range" , ID "x" , ID "chanX" ]) `shouldPrint`
      "x := range chanX"

    it "full range" $
      print (L [ KW "range" , ID "k" , ID "v" , ID "users" ]) `shouldPrint`
      "k, v := range users"

    it "slice from" $
      print (L [ KW "slice" , ID "xs" , IL 1 , ID "_" ]) `shouldPrint` "xs[1:]"

    it "slice to" $
      print (L [ KW "slice" , ID "xs" , ID "_" , IL 4 ]) `shouldPrint` "xs[:4]"

    it "slice from-to" $
      print (L [ KW "slice" , ID "indexes" , ID "i" , L [ OP "+" , ID "j" , IL 1 ] ])
      `shouldPrint` "indexes[i:j + 1]"

    it "empty struct" $
      print (L [ KW "struct" , ID "foo" ])
      `shouldPrint` "type foo struct{}"

    it "single entry struct" $
      print (L [ KW "struct" , ID "List" , L [ ID "xs" , L [ TP "slice" , TP "string" ] ] ])
      `shouldPrint` "type List struct {\nxs []string\n}"

    it "multiple entries struct" $
      print (L [ KW "struct" , ID "point"
        , L [ ID "x" , TP "int" ]
        , L [ ID "y" , TP "int" ] ])
      `shouldPrint` "type point struct {\nx int\ny int\n}"

    it "empty interface" $
      print (L [ KW "interface" , ID "foo" ])
      `shouldPrint` "type foo interface{}"

    it "single entry interface" $
      print (L [ KW "interface" , ID "Printer" ,
        L [ ID "Print" , L [ L [ ID "x" , TP "any" ]] ] ])
      `shouldPrint` "type Printer interface {\nPrint(x interface{})\n}"

    it "multiple entries interface" $
      print (L [ KW "interface" , ID "shape"
        , L [ ID "area" , Nil , TP "double" ]
        , L [ ID "perimeter" , Nil , TP "double" ] ])
      `shouldPrint` "type shape interface {\narea() double\nperimeter() double\n}"

    it "forever for loop" $
      print (L [ KW "for" , L [ ID "fmt.Println" , SL "fizz" ] ]) `shouldPrint`
      "for {\nfmt.Println(\"fizz\")\n}"

    it "standard for loop" $
      print (L [ KW "for" , ID "i" , IL 0 , IL 10 , L [ ID "fmt.Println" , SL "buzz" ] ])
      `shouldPrint`
      "for i := 0; i < 10; i++ {\nfmt.Println(\"buzz\")\n}"

    it "only condition for loop" $
      print (L [ KW "for" , L [ OP "<" , ID "n" , IL 42 ] , L [ ID "fmt.Print" , SL "?" ] ])
      `shouldPrint`
      "for n < 42 {\nfmt.Print(\"?\")\n}"

    it "custom for loop" $
      print (L [ KW "for" , ID "x" , ID "k" , BL True
      , L [ KW "set" , ID "x" , L [ OP "+", ID "x", ID "foo.N" ] ]
      , L [ KW "break" ] ])
      `shouldPrint`
      "for x := k; true; x = x + foo.N {\nbreak\n}"


  describe "Fun.Printer.singleLine" $ do

    it "==" $
      singleLine (L [ OP "=" , ID "foo" , ID "bar" ]) `shouldBe`
      "(= foo bar)"

    it "var" $
      singleLine (L [ KW "var" , ID "foo" , TP "int" , IL 42 ]) `shouldBe`
      "(var foo :int 42)"


  describe "Fun.Desugar.desugar" $ do
    it "does nothing when there is nothing to do" $
      desugar (L [ ID "foo" , ID "bar" ]) `shouldBe` L [ ID "foo" , ID "bar" ]

    it "print" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "print" , SL "hello world"] ]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , SL "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Println" , SL "hello world"] ]]

    it "printf" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "printf" , SL "<%s>\\n" , SL "hello world" ] ]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , SL "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Printf" , SL "<%s>\\n", SL "hello world" ] ]]

    it "print with existing import" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "import" , SL "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ KW "print" , SL "hello world" ] ]])
      `shouldBe` L
        [ KW "package" , ID "main" , L
        [ KW "import" , SL "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Println" , SL "hello world" ] ]]

    it "deeper nested print" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "for" , L
            [ KW "print" , SL "hello world" ] ]]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , SL "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ KW "for" , L
            [ ID "fmt.Println" , SL "hello world" ] ]]]

    it "print and printf" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ L [ KW "print" , SL "<first>" ]
          , L [ KW "printf" , SL "<%s>\\n" , SL "second" ] ]]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , SL "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ L [ ID "fmt.Println" , SL "<first>" ]
          , L [ ID "fmt.Printf" , SL "<%s>\\n", SL "second" ] ]]]

    it "adds return to constant func" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "Version" , Nil , TP "string" ,
          SL "v1.02" ] ])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "Version" , Nil , TP "string" ,
          L [ KW "return" , SL "v1.02" ] ]]

    it "adds return to func with declared results" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ OP "*" , ID "x" , IL 2 ] ]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ OP "*" , ID "x" , IL 2 ] ] ]]

    it "adds return to the last expression" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" , L
          [ L [ KW "var" , ID "y" , L [ OP "*" , ID "x" , IL 2 ] ]
          , ID "y" ]]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" , L
          [ L [ KW "var" , ID "y" , L [ OP "*" , ID "x" , IL 2 ] ]
          , L [ KW "return" , ID "y" ] ]]]

    it "does not add return if already present" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ OP "*" , ID "x" , IL 2 ] ] ]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ OP "*" , ID "x" , IL 2 ] ] ]]


integrationTests :: Spec
integrationTests = describe "Go.Fmt.gofmt" $ do

    it "formats valid code" $
      gofmt "func  foo  (  ) { \n i++}" `shouldPrint` "func foo() {\n\ti++\n}"

    it "returns err on a broken code" $
      gofmt "func foo }( __" `shouldBe` Left (GoError "1:20: expected '(', found '}'")

    it "unError GoError" $
      mapLeft unError (gofmt "func foo }( __") `shouldBe` Left "Go error: 1:20: expected '(', found '}'"


dummyTests :: Spec
dummyTests = describe "MOAR coverage!" $ do

  it "compare L A" $
    compare (L [ID "foo"]) (ID "bar") `shouldBe` GT

  it "compare A L" $
    compare (ID "foo") (L [ID "bar"]) `shouldBe` LT
