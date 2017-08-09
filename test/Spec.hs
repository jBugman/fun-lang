{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
module Main where

import ClassyPrelude           hiding (LT)
import Data.Either.Combinators (mapLeft)
import Test.Hspec              (Spec, describe, hspec, it, shouldBe)

import qualified Data.Ord as Ord

import Foreign.Gofmt     (gofmt)
import Fun               (translate)
import Fun.Desugar       (desugar)
import Fun.Errors        (Error (..), Pos (..), unError)
import Fun.Go.Printer    (printGo)
import Fun.PrettyPrinter (singleLine)
import Fun.SExpression   (pattern ID, pattern KW, pattern L, pattern LT, Literal (..), pattern Nil,
                          pattern TP)
import Test.Utils        (int, op, shouldPrint, str, translationExample)


main :: IO ()
main = hspec . describe "Everything" $ do
  dummyTests
  describe "Tests" $ do
    describe "Unit"        unitTests
    describe "Integration" integrationTests
  examples


examples :: Spec
examples = describe "Examples" $ do
  translationExample "01_hello_world"
  translationExample "02_values"
  translationExample "03_variables"
  translationExample "04_for"
  translationExample "05_if_else"
  translationExample "06_switch"
  translationExample "07_slices"
  translationExample "08_maps"
  translationExample "09_range"
  translationExample "10_functions"
  translationExample "11_closures"
  translationExample "12_pointers"
  translationExample "13_structs"
  translationExample "14_methods"
  translationExample "15_interfaces"


unitTests :: Spec
unitTests = do

  describe "Fun.Go.Printer.printGo" $ do

    it "fail on standalone Nil" $
      mapLeft unError (printGo Nil)
      `shouldBe`
      Left "translation error: not supported yet: ()"

    it "import" $
      printGo (L [ KW "import" , str "fmt" ])
      `shouldPrint`
      "import \"fmt\""

    it "import with alias" $
      printGo (L [ KW "import" , str "very/long-package" , str "pkg" ])
      `shouldPrint`
      "import pkg \"very/long-package\""

    it "simple func" $
      printGo (L [ KW "func" , ID "setS" , L [ KW "set" , ID "s" , int 2 ] ])
      `shouldPrint`
      "func setS() {\n  s = 2\n}"

    it "<" $
      printGo (L [ op "<" , ID "n" , int 10 ]) `shouldPrint` "n < 10"

    it "==" $
      printGo (L [ op "==" , ID "foo" , ID "bar" ]) `shouldPrint` "foo == bar"

    it "--" $
      printGo (L [ op "--" , ID "j" ]) `shouldPrint` "j--"

    it "* as product" $
      printGo (L [ op "*" , ID "x" , ID "y" ]) `shouldPrint` "x * y"

    it "* as pointer" $
      printGo (L [ op "*" , ID "foo" ]) `shouldPrint` "*foo"

    it "hex == char" $
      printGo (L
        [ op "=="
        , LT (Integer 16 1)
        , LT (Char "a")
        ]) `shouldPrint` "0x1 == 'a'"

    it "double" $
      printGo (LT (Double 9.99)) `shouldPrint` "9.99"

    it "oct" $
      printGo (LT (Integer 8 0o644)) `shouldPrint` "0644"

    it "complex" $
      printGo (L [ TP "complex" , LT (Double 5.2) , int 3 ]) `shouldPrint` "5.2+3i"

    it "complex negative" $
      printGo (L [ TP "complex" , int 1 , int (-2) ]) `shouldPrint` "1-2i"

    it "string" $
      printGo (str "fizzbuzz") `shouldPrint` "\"fizzbuzz\""

    it "'any' type" $
      printGo (TP "any") `shouldPrint` "interface{}"

    it "const decl" $
      printGo (L [ KW "const" , ID "a" , str "initial" ])
      `shouldPrint`
      "const a = \"initial\""

    it "full const decl" $
      printGo (L [ KW "const" , ID "a" , TP "State" , LT (Integer 16 0xA) ])
      `shouldPrint`
      "const a State = 0xa"

    it "full var decl" $
      printGo (L [ KW "var" , ID "b" , TP "int" , int 1 ]) `shouldPrint` "var b int = 1"

    it "infer type var" $
      printGo (L [ KW "var" , ID "d" , LT (Bool True) ]) `shouldPrint` "var d = true"

    it "zero value var" $
      printGo (L [ KW "var" , ID "x" , TP "int" ]) `shouldPrint` "var x int"

    it "var from a function" $
      printGo (L [ KW "var" , ID "foo" , L [ ID "calc" , ID "x" ] ])
      `shouldPrint`
      "var foo = calc(x)"

    it "two vars from a function" $
      printGo (L [ KW "var" , ID "x" , ID "y" , L [ ID "factory" , ID "z" ] ])
      `shouldPrint`
      "var x, y = factory(z)"

    it "var x, y = m[2]" $
      printGo (L [ KW "var" , ID "x" , ID "y" , L [ KW "val" , ID "m" , int 2 ] ])
      `shouldPrint`
      "var x, y = m[2]"

    it "var _, x = m[2]" $
      printGo (L [ KW "var" , ID "_" , ID "x" , L [ KW "val" , ID "m" , int 2 ] ])
      `shouldPrint`
      "var _, x = m[2]"

    it "zero value slice var" $
      printGo (L [ KW "var" , ID "x" , L [ TP "slice" , TP "string" ] ])
      `shouldPrint`
      "var x []string"

    it "zero value map var" $
      printGo (L
        [ KW "var" , ID "counters"
        , L [ TP "map" , TP "string" , TP "int" ] ])
      `shouldPrint`
      "var counters map[string]int"

    -- spellchecker:ignore nums
    it "var with slice literal initializer" $
      printGo (L
        [ KW "var" , ID "nums"
        , L [ L [ TP "slice" , TP "int" ]
        , L [ int 1 , int 2, int 3 ] ] ])
      `shouldPrint`
      "var nums = []int{1, 2, 3}"

    it "var with map literal initializer" $
      printGo (L [ KW "var" , ID "m" , L
      [ L [ TP "map" , TP "string" , TP "bool" ] , L
        [ L [ str "foo", LT (Bool True) ]
        , L [ str "bar" , LT (Bool False) ] ]]])
      `shouldPrint`
      "var m = map[string]bool{\"foo\": true, \"bar\": false}"

    it "slice of slices" $
      printGo (L [ TP "slice" , L [ TP "slice" , TP "string" ] ])
      `shouldPrint`
      "[][]string"

    it "map lookup" $
      printGo (L [ KW "set" , ID "_" , ID "ok" , L [ KW "val" , ID "m" , str "Bob" ] ])
      `shouldPrint`
      "_, ok = m[\"Bob\"]"

    it "slice index" $
      printGo (L [ KW "val" , ID "xs" , int 4 ]) `shouldPrint` "xs[4]"

    it "slice 2D-index" $
      printGo (L [ KW "val" , ID "xs" , ID "y" , ID "x" ]) `shouldPrint` "xs[y][x]"

    it "short range" $
      printGo (L [ KW "range" , ID "x" , ID "chanX" ]) `shouldPrint`
      "x := range chanX"

    it "full range" $
      printGo (L [ KW "range" , ID "k" , ID "v" , ID "users" ]) `shouldPrint`
      "k, v := range users"

    it "slice from" $
      printGo (L [ KW "slice" , ID "xs" , int 1 , ID "_" ]) `shouldPrint` "xs[1:]"

    it "slice to" $
      printGo (L [ KW "slice" , ID "xs" , ID "_" , int 4 ])
      `shouldPrint`
      "xs[:4]"

    it "slice from-to" $
      printGo (L [ KW "slice" , ID "indexes" , ID "i" , L [ op "+" , ID "j" , int 1 ] ])
      `shouldPrint`
      "indexes[i:j + 1]"

    it "empty struct" $
      printGo (L [ KW "struct" , ID "foo" ])
      `shouldPrint`
      "type foo struct{}"

    it "single entry struct" $
      printGo (L [ KW "struct" , ID "List" , L
        [ ID "xs" , L [ TP "slice" , TP "string" ] ] ])
      `shouldPrint`
      "type List struct {\n  xs []string\n}"

    it "multiple entries struct" $
      printGo (L [ KW "struct" , ID "point"
        , L [ ID "x" , TP "int" ]
        , L [ ID "y" , TP "int" ] ])
      `shouldPrint` "type point struct {\n  x int\n  y int\n}"

    it "struct embedding id" $
      printGo (L [ KW "struct" , ID "point3D"
        , TP "point"
        , L [ ID "z" , TP "int" ] ])
      `shouldPrint` "type point3D struct {\n  point\n  z int\n}"

    it "struct embedding pointer" $
      printGo (L [ KW "struct" , ID "foo"
        , L [ TP "ptr" , TP "T" ]
        , L [ ID "x" , TP "int" ] ])
      `shouldPrint` "type foo struct {\n  *T\n  x int\n}"

    it "empty interface" $
      printGo (L [ KW "interface" , ID "foo" ])
      `shouldPrint` "type foo interface{}"

    it "single entry interface" $
      printGo (L [ KW "interface" , ID "Printer" ,
        L [ ID "Print" , L [ L [ ID "x" , TP "any" ]] ] ])
      `shouldPrint` "type Printer interface {\n  Print(x interface{})\n}"

    it "interface entry with slice arg" $
      printGo (L [ KW "interface" , ID "foo" ,
        L [ ID "bar" , L [ L [ TP "slice" , TP "string" ] ] ] ])
      `shouldPrint` "type foo interface {\n  bar([]string)\n}"

    it "multiple entries interface" $
      printGo (L [ KW "interface" , ID "shape"
        , L [ ID "area" , Nil , TP "double" ]
        , L [ ID "perimeter" , Nil , TP "double" ] ])
      `shouldPrint` "type shape interface {\n  area() double\n  perimeter() double\n}"

    it "embedded interface" $
      printGo (L [ KW "interface" , ID "foo"
        , TP "io.Writer"
        , L [ ID "bar" , Nil ] ])
      `shouldPrint` "type foo interface {\n  io.Writer\n  bar()\n}"

    it "forever for loop" $
      printGo (L [ KW "for" , L [ ID "fmt.Println" , str "fizz" ] ])
      `shouldPrint`
      "for {\n  fmt.Println(\"fizz\")\n}"

    it "standard for loop" $
      printGo (L [ KW "for" , ID "i" , int 0 , int 10 , L [ ID "fmt.Println" , str "buzz" ] ])
      `shouldPrint`
      "for i := 0; i < 10; i++ {\n  fmt.Println(\"buzz\")\n}"

    it "only condition for loop" $
      printGo (L [ KW "for"
        , L [ op "<" , ID "n" , int 42 ]
        , L [ ID "fmt.Print" , str "?" ] ])
      `shouldPrint`
      "for n < 42 {\n  fmt.Print(\"?\")\n}"

    it "custom for loop" $
      printGo (L [ KW "for" , ID "x" , ID "k" , LT (Bool True)
      , L [ KW "set" , ID "x" , L [ op "+", ID "x", ID "foo.N" ] ]
      , L [ KW "break" ] ])
      `shouldPrint`
      "for x := k; true; x = x + foo.N {\n  break\n}"

    it "minimal func literal" $
      printGo (L [ KW "func" , Nil , L [ ID "pass" , int 42 ] ])
      `shouldPrint`
      "func() {\n  pass(42)\n}"

    it "func literal with result" $
      printGo (L [ KW "func" , Nil , TP "int" , L
      [ L [ KW "set" , ID "i" , L [ op "+", ID "i", int 1 ] ]
      , L [ KW "return" , ID "i" ] ]])
      `shouldPrint`
      "func() int {\n  i = i + 1\n  return i\n}"

    it "func literal with args" $
      printGo (L [ KW "func" , L [ L [ ID "x" , TP "int" ]] , L
      [ L [ KW "set" , ID "i" , L [ op "+", ID "i", ID "x" ] ]
      , L [ KW "return" , ID "i" ] ]])
      `shouldPrint`
      "func(x int) {\n  i = i + x\n  return i\n}"

    it "minimal func type lit" $
       printGo (L [ TP "func" , Nil ]) `shouldPrint` "func()"

    it "func type lit with result" $
       printGo (L [ TP "func" , Nil , TP "int" ]) `shouldPrint` "func() int"

    it "func type lit with args" $
       printGo (L [ TP "func" , L [ TP "int" ] ]) `shouldPrint` "func(int)"

    it "func returning func A" $
       printGo (L [ KW "func" , ID "foo" , Nil
       , L [ TP "func" , Nil , TP "int" ]
       , L [ KW "return" , ID "bar" ] ])
       `shouldPrint`
       "func foo() func() int {\n  return bar\n}"

    it "func returning func B" $
       printGo (L [ KW "func" , ID "foo" , Nil
       , L [ L [ TP "func" , Nil , TP "int" ] ]
       , L [ KW "return" , ID "bar" ] ])
       `shouldPrint`
       "func foo() (func() int) {\n  return bar\n}"

    it "type alias" $
       printGo (L [ KW "alias" , ID "UserID" , TP "string" ])
       `shouldPrint`
       "type UserID string"

    it "two-var function result" $
      printGo (L [ KW "set" , ID "x" , ID "ok" , L [ ID "os.LookupEnv" , ID "name" ] ])
      `shouldPrint`
      "x, ok = os.LookupEnv(name)"

    it "slice elem set" $
      printGo (L [ KW "set" , L [ KW "val" , ID "names" , int 5 ] , str "Bob" ])
      `shouldPrint`
      "names[5] = \"Bob\""

    it "empty struct literal" $
      printGo (L [ TP "foo" ]) `shouldPrint` "foo{}"

    -- spellchecker:ignore SJFKD
    it "struct literal" $
      printGo (L [ TP "api" , L [ L [ ID "key" , str "SJFKD" ] ] ])
      `shouldPrint`
      "api{key: \"SJFKD\"}"

    it "var from struct literal" $
      printGo (L [ KW "var" , ID "x" , L [ TP "api" , L [ L
        [ ID "key" , str "SJFKD" ] ] ] ])
      `shouldPrint`
      "var x = api{key: \"SJFKD\"}"

    it "func foo() {}" $
      printGo (L [ KW "func" , ID "foo" , Nil , Nil ])
      `shouldPrint`
      "func foo() {}"

    it "func f(xs []string) int {...}" $
      printGo (L [ KW "func" , ID "f"
        , L [ L [ ID "xs" , L [ TP "slice" , TP "string" ] ] ]
        , TP "int"
        , L [ KW "return" , int 42 ] ])
      `shouldPrint`
      "func f(xs []string) int {\n  return 42\n}"

    -- spellchecker:ignore boop
    it "func foo() {...}" $
      printGo (L [ KW "func" , ID "foo" , Nil , L
      [ L [ KW "var" , ID "x" , int 5 ]
      , L [ ID "boop" , ID "x" ] ] ])
      `shouldPrint`
      "func foo() {\n  var x = 5\n  boop(x)\n}"

    it "method (x bar) foo() {}" $
      printGo (L [ KW "method" , L [ ID "x" , TP "bar" ] , ID "foo" , Nil , Nil ])
      `shouldPrint`
      "func (x bar) foo() {}"

    it "method (bar) foo() {} #1" $
      printGo (L [ KW "method" , TP "bar" , ID "foo" , Nil ])
      `shouldPrint`
      "func (bar) foo() {}"

    it "method (bar) foo() {} #2" $
      printGo (L [ KW "method" , TP "bar" , ID "foo" , Nil , Nil ])
      `shouldPrint`
      "func (bar) foo() {}"

    it "func (b *bar) foo() {...}" $
      printGo (L [ KW "method"
        , L [ ID "b" , L [ TP "ptr" , TP "bar" ] ]
        , ID "foo" , Nil , L
          [ L [ KW "var" , ID "x" , L [ op "+" , ID "b" , int 3 ] ]
          , L [ ID "boop" , ID "x" ] ] ])
      `shouldPrint`
      "func (b *bar) foo() {\n  var x = b + 3\n  boop(x)\n}"

    it "x = string(y)" $
      printGo (L [ KW "set" , ID "x" , L [ KW "cast" , TP "string" , ID "y" ] ])
      `shouldPrint`
      "x = string(y)"

    it "x = []byte(y)" $
      printGo (L [ KW "set" , ID "x" , L
        [ KW "cast" , L [ TP "slice" , TP "byte" ] , ID "y" ] ])
      `shouldPrint`
      "x = []byte(y)"

    it "x = y.(Foo)" $
      printGo (L [ KW "set" , ID "x" , L [ KW "assert" , TP "Foo" , ID "y" ] ])
      `shouldPrint`
      "x = y.(Foo)"

    it "slice literal" $
      printGo (L [ L [ TP "slice" , TP "int" ] , L [ int 1 , int 2 , int 3 ] ])
      `shouldPrint`
      "[]int{1, 2, 3}"

    it "map literal" $
      printGo (L
        [ L [ TP "map" , TP "foo" , TP "int" ] , L
          [ L [ str "foo" , int 42 ]
          , L [ str "bar" , int 3 ] ] ])
      `shouldPrint`
      "map[foo]int{\"foo\": 42, \"bar\": 3}"

    it "map of slices" $
      printGo (L [ TP "map" , TP "q" , L [ TP "slice" , TP "a" ] ])
      `shouldPrint`
      "map[q][]a"

    it "map of pointers" $
      printGo (L [ TP "map" , TP "q" , L [ TP "ptr" , TP "a" ] ])
      `shouldPrint`
      "map[q]*a"

    it "map of maps" $
      printGo (L [ TP "map" , TP "x" , L [ TP "map" , TP "y" , TP "z" ] ])
      `shouldPrint`
      "map[x]map[y]z"

    it "call chain A" $
      printGo (L [ op "."
        , ID "c"
        , L [ ID "Echo" ]
        , L [ ID "URI" , ID "bot.handleSession" ] ])
      `shouldPrint`
      "c.Echo().URI(bot.handleSession)"

    it "call chain B" $
      printGo (L [ op "==" , int 0 , L
        [ op "."
        , L [ KW "val" , ID "suite.t.Messages" , ID "userID" ]
        , L [ ID "Size" ] ] ])
      `shouldPrint`
      "0 == suite.t.Messages[userID].Size()"

    it "switch simple" $
      printGo (L [ KW "switch" , ID "i" , L
        [ L [ KW "case" , int 1 , L [ ID "foo" , str "one" ] ]
        , L [ KW "case" , int 2 , L [ ID "bar" , str "two" ] ] ]])
      `shouldPrint`
      "switch i {\n  case 1: foo(\"one\")\n  case 2: bar(\"two\")\n}"

    it "switch no-expr default" $
      printGo (L [ KW "switch" , L
        [ L [ KW "case" , L [ op "==" , ID "i" , int 1 ] , L [ ID "foo" , str "one" ] ]
        , L [ KW "default" , L [ ID "bar" , str "two" ] ] ]])
      `shouldPrint`
      "switch {\n  case i == 1: foo(\"one\")\n  default: bar(\"two\")\n}"

    it "switch empty case" $
      printGo (L [ KW "switch" , ID "i" , L
        [ L [ KW "case" , int 0 ]
        , L [ KW "default" , L [ ID "bar" , str "two" ] ] ]])
      `shouldPrint`
      "switch i {\n  case 0:\n  default: bar(\"two\")\n}"

    it "make chan" $
      printGo (L [ KW "make" , L [ TP "chan" , TP "int" ] ])
      `shouldPrint`
      "make(chan int)"

    it "bidirectional chan" $
      printGo (L [ TP "chan" , TP "int" ]) `shouldPrint` "chan int"

    -- it "send chan type" $
    --   printGo (L [ TP "<-chan" , TP "int" ]) `shouldPrint` "<-chan int"

    -- it "receive chan type" $
    --   printGo (L [ TP "chan<-" , TP "int" ]) `shouldPrint` "chan<- int"


  describe "Fun.Printer.singleLine" $ do

    it "==" $
      singleLine (L [ op "=" , ID "foo" , ID "bar" ]) `shouldBe`
      "(= foo bar)"

    it "var" $
      singleLine (L [ KW "var" , ID "foo" , TP "int" , int 42 ]) `shouldBe`
      "(var foo :int 42)"


  describe "Fun.Desugar.desugar" $ do
    it "does nothing when there is nothing to do" $
      desugar (L [ ID "foo" , ID "bar" ]) `shouldBe` L [ ID "foo" , ID "bar" ]

    it "print" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "print" , str "hello world" ] ]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Println" , str "hello world" ] ]]

    it "printf" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "printf" , str "<%s>\\n" , str "hello world" ] ]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Printf" , str "<%s>\\n" , str "hello world" ] ]]

    it "print with existing import" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ KW "print" , str "hello world" ] ]])
      `shouldBe` L
        [ KW "package" , ID "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Println" , str "hello world" ] ]]

    it "deeper nested print" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "for" , L
            [ KW "print" , str "hello world" ] ]]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ KW "for" , L
            [ ID "fmt.Println" , str "hello world" ] ]]]

    it "print and printf" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ L [ KW "print" , str "<first>" ]
          , L [ KW "printf"
              , str "<%s>\\n"
              , str "second" ] ]]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , ID "main" , L
          [ L [ ID "fmt.Println" , str "<first>" ]
          , L [ ID "fmt.Printf"
              , str "<%s>\\n"
              , str "second" ] ]]]

    it "adds return to constant func" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "Version" , Nil , TP "string" ,
          str "v1.02" ] ])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "Version" , Nil , TP "string" ,
          L [ KW "return" , str "v1.02" ] ]]

    it "adds return to func with declared results" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ op "*" , ID "x" , int 2 ] ]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ op "*" , ID "x" , int 2 ] ] ]]

    it "adds return to the last expression" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" , L
          [ L [ KW "var" , ID "y" , L [ op "*" , ID "x" , int 2 ] ]
          , ID "y" ]]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" , L
          [ L [ KW "var" , ID "y" , L [ op "*" , ID "x" , int 2 ] ]
          , L [ KW "return" , ID "y" ] ]]]

    it "does not add return if already present" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ op "*" , ID "x" , int 2 ] ] ]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ op "*" , ID "x" , int 2 ] ] ]]


integrationTests :: Spec
integrationTests = do

  describe "Fmt.translate" .

    it "translates simple expression" $
      translate "(func foo (bar x 42))"
      `shouldPrint`
      "func foo() {\n  bar(x, 42)\n}"


  describe "Go.Fmt.gofmt" $ do

    it "formats valid code" $
      gofmt "func  foo  (  ) { \n i++}" `shouldPrint` "func foo() {\n\ti++\n}"

    it "returns err on a broken code" $
      gofmt "func foo }( __"
      `shouldBe`
      Left (GoError (Just (Pos 1 20)) "expected '(', found '}'")

    it "unError GoError" $
      mapLeft unError (gofmt "func foo }( __")
      `shouldBe`
      Left "1:20: Go error: expected '(', found '}'"


dummyTests :: Spec
dummyTests = describe "MOAR coverage!" $ do

  it "compare L A" $
    compare (L [ID "foo"]) (ID "bar") `shouldBe` Ord.GT

  it "compare A L" $
    compare (ID "foo") (L [ID "bar"]) `shouldBe` Ord.LT
