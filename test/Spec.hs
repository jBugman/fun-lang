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
import Fun.SExpression   (pattern KW, pattern L, pattern LT, Literal (..), pattern Nil)
import Test.Utils        (i, int, op, shouldPrint, str, tp, translationExample)


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
      printGo (L [ KW "func" , i "setS" , L [ KW "set" , i "s" , int 2 ] ])
      `shouldPrint`
      "func setS() {\n  s = 2\n}"

    it "<" $
      printGo (L [ op "<" , i "n" , int 10 ]) `shouldPrint` "n < 10"

    it "==" $
      printGo (L [ op "==" , i "foo" , i "bar" ]) `shouldPrint` "foo == bar"

    it "--" $
      printGo (L [ op "--" , i "j" ]) `shouldPrint` "j--"

    it "* as product" $
      printGo (L [ op "*" , i "x" , i "y" ]) `shouldPrint` "x * y"

    it "* as pointer" $
      printGo (L [ op "*" , i "foo" ]) `shouldPrint` "*foo"

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
      printGo (L [ tp "complex" , LT (Double 5.2) , int 3 ]) `shouldPrint` "5.2+3i"

    it "complex negative" $
      printGo (L [ tp "complex" , int 1 , int (-2) ]) `shouldPrint` "1-2i"

    it "string" $
      printGo (str "fizzbuzz") `shouldPrint` "\"fizzbuzz\""

    it "'any' type" $
      printGo (tp "any") `shouldPrint` "interface{}"

    it "const decl" $
      printGo (L [ KW "const" , i "a" , str "initial" ])
      `shouldPrint`
      "const a = \"initial\""

    it "full const decl" $
      printGo (L [ KW "const" , i "a" , tp "State" , LT (Integer 16 0xA) ])
      `shouldPrint`
      "const a State = 0xa"

    it "full var decl" $
      printGo (L [ KW "var" , i "b" , tp "int" , int 1 ]) `shouldPrint` "var b int = 1"

    it "infer type var" $
      printGo (L [ KW "var" , i "d" , LT (Bool True) ]) `shouldPrint` "var d = true"

    it "zero value var" $
      printGo (L [ KW "var" , i "x" , tp "int" ]) `shouldPrint` "var x int"

    it "var from a function" $
      printGo (L [ KW "var" , i "foo" , L [ i "calc" , i "x" ] ])
      `shouldPrint`
      "var foo = calc(x)"

    it "two vars from a function" $
      printGo (L [ KW "var" , i "x" , i "y" , L [ i "factory" , i "z" ] ])
      `shouldPrint`
      "var x, y = factory(z)"

    it "var x, y = m[2]" $
      printGo (L [ KW "var" , i "x" , i "y" , L [ KW "val" , i "m" , int 2 ] ])
      `shouldPrint`
      "var x, y = m[2]"

    it "var _, x = m[2]" $
      printGo (L [ KW "var" , i "_" , i "x" , L [ KW "val" , i "m" , int 2 ] ])
      `shouldPrint`
      "var _, x = m[2]"

    it "zero value slice var" $
      printGo (L [ KW "var" , i "x" , L [ tp "slice" , tp "string" ] ])
      `shouldPrint`
      "var x []string"

    it "zero value map var" $
      printGo (L
        [ KW "var" , i "counters"
        , L [ tp "map" , tp "string" , tp "int" ] ])
      `shouldPrint`
      "var counters map[string]int"

    -- spellchecker:ignore nums
    it "var with slice literal initializer" $
      printGo (L
        [ KW "var" , i "nums"
        , L [ L [ tp "slice" , tp "int" ]
        , L [ int 1 , int 2, int 3 ] ] ])
      `shouldPrint`
      "var nums = []int{1, 2, 3}"

    it "var with map literal initializer" $
      printGo (L [ KW "var" , i "m" , L
      [ L [ tp "map" , tp "string" , tp "bool" ] , L
        [ L [ str "foo", LT (Bool True) ]
        , L [ str "bar" , LT (Bool False) ] ]]])
      `shouldPrint`
      "var m = map[string]bool{\"foo\": true, \"bar\": false}"

    it "slice of slices" $
      printGo (L [ tp "slice" , L [ tp "slice" , tp "string" ] ])
      `shouldPrint`
      "[][]string"

    it "map lookup" $
      printGo (L [ KW "set" , i "_" , i "ok" , L [ KW "val" , i "m" , str "Bob" ] ])
      `shouldPrint`
      "_, ok = m[\"Bob\"]"

    it "slice index" $
      printGo (L [ KW "val" , i "xs" , int 4 ]) `shouldPrint` "xs[4]"

    it "slice 2D-index" $
      printGo (L [ KW "val" , i "xs" , i "y" , i "x" ]) `shouldPrint` "xs[y][x]"

    it "short range" $
      printGo (L [ KW "range" , i "x" , i "chanX" ]) `shouldPrint`
      "x := range chanX"

    it "full range" $
      printGo (L [ KW "range" , i "k" , i "v" , i "users" ]) `shouldPrint`
      "k, v := range users"

    it "slice from" $
      printGo (L [ KW "slice" , i "xs" , int 1 , i "_" ]) `shouldPrint` "xs[1:]"

    it "slice to" $
      printGo (L [ KW "slice" , i "xs" , i "_" , int 4 ])
      `shouldPrint`
      "xs[:4]"

    it "slice from-to" $
      printGo (L [ KW "slice" , i "indexes" , i "i" , L [ op "+" , i "j" , int 1 ] ])
      `shouldPrint`
      "indexes[i:j + 1]"

    it "empty struct" $
      printGo (L [ KW "struct" , i "foo" ])
      `shouldPrint`
      "type foo struct{}"

    it "single entry struct" $
      printGo (L [ KW "struct" , i "List" , L
        [ i "xs" , L [ tp "slice" , tp "string" ] ] ])
      `shouldPrint`
      "type List struct {\n  xs []string\n}"

    it "multiple entries struct" $
      printGo (L [ KW "struct" , i "point"
        , L [ i "x" , tp "int" ]
        , L [ i "y" , tp "int" ] ])
      `shouldPrint` "type point struct {\n  x int\n  y int\n}"

    it "struct embedding id" $
      printGo (L [ KW "struct" , i "point3D"
        , tp "point"
        , L [ i "z" , tp "int" ] ])
      `shouldPrint` "type point3D struct {\n  point\n  z int\n}"

    it "struct embedding pointer" $
      printGo (L [ KW "struct" , i "foo"
        , L [ tp "ptr" , tp "T" ]
        , L [ i "x" , tp "int" ] ])
      `shouldPrint` "type foo struct {\n  *T\n  x int\n}"

    it "empty interface" $
      printGo (L [ KW "interface" , i "foo" ])
      `shouldPrint` "type foo interface{}"

    it "single entry interface" $
      printGo (L [ KW "interface" , i "Printer" ,
        L [ i "Print" , L [ L [ i "x" , tp "any" ]] ] ])
      `shouldPrint` "type Printer interface {\n  Print(x interface{})\n}"

    it "interface entry with slice arg" $
      printGo (L [ KW "interface" , i "foo" ,
        L [ i "bar" , L [ L [ tp "slice" , tp "string" ] ] ] ])
      `shouldPrint` "type foo interface {\n  bar([]string)\n}"

    it "multiple entries interface" $
      printGo (L [ KW "interface" , i "shape"
        , L [ i "area" , Nil , tp "double" ]
        , L [ i "perimeter" , Nil , tp "double" ] ])
      `shouldPrint` "type shape interface {\n  area() double\n  perimeter() double\n}"

    it "embedded interface" $
      printGo (L [ KW "interface" , i "foo"
        , tp "io.Writer"
        , L [ i "bar" , Nil ] ])
      `shouldPrint` "type foo interface {\n  io.Writer\n  bar()\n}"

    it "forever for loop" $
      printGo (L [ KW "for" , L [ i "fmt.Println" , str "fizz" ] ])
      `shouldPrint`
      "for {\n  fmt.Println(\"fizz\")\n}"

    it "standard for loop" $
      printGo (L [ KW "for" , i "i" , int 0 , int 10 , L [ i "fmt.Println" , str "buzz" ] ])
      `shouldPrint`
      "for i := 0; i < 10; i++ {\n  fmt.Println(\"buzz\")\n}"

    it "only condition for loop" $
      printGo (L [ KW "for"
        , L [ op "<" , i "n" , int 42 ]
        , L [ i "fmt.Print" , str "?" ] ])
      `shouldPrint`
      "for n < 42 {\n  fmt.Print(\"?\")\n}"

    it "custom for loop" $
      printGo (L [ KW "for" , i "x" , i "k" , LT (Bool True)
      , L [ KW "set" , i "x" , L [ op "+", i "x", i "foo.N" ] ]
      , L [ KW "break" ] ])
      `shouldPrint`
      "for x := k; true; x = x + foo.N {\n  break\n}"

    it "minimal func literal" $
      printGo (L [ KW "func" , Nil , L [ i "pass" , int 42 ] ])
      `shouldPrint`
      "func() {\n  pass(42)\n}"

    it "func literal with result" $
      printGo (L [ KW "func" , Nil , tp "int" , L
      [ L [ KW "set" , i "i" , L [ op "+", i "i", int 1 ] ]
      , L [ KW "return" , i "i" ] ]])
      `shouldPrint`
      "func() int {\n  i = i + 1\n  return i\n}"

    it "func literal with args" $
      printGo (L [ KW "func" , L [ L [ i "x" , tp "int" ]] , L
      [ L [ KW "set" , i "i" , L [ op "+", i "i", i "x" ] ]
      , L [ KW "return" , i "i" ] ]])
      `shouldPrint`
      "func(x int) {\n  i = i + x\n  return i\n}"

    it "minimal func type lit" $
       printGo (L [ tp "func" , Nil ]) `shouldPrint` "func()"

    it "func type lit with result" $
       printGo (L [ tp "func" , Nil , tp "int" ]) `shouldPrint` "func() int"

    it "func type lit with args" $
       printGo (L [ tp "func" , L [ tp "int" ] ]) `shouldPrint` "func(int)"

    it "func returning func A" $
       printGo (L [ KW "func" , i "foo" , Nil
       , L [ tp "func" , Nil , tp "int" ]
       , L [ KW "return" , i "bar" ] ])
       `shouldPrint`
       "func foo() func() int {\n  return bar\n}"

    it "func returning func B" $
       printGo (L [ KW "func" , i "foo" , Nil
       , L [ L [ tp "func" , Nil , tp "int" ] ]
       , L [ KW "return" , i "bar" ] ])
       `shouldPrint`
       "func foo() (func() int) {\n  return bar\n}"

    it "type alias" $
       printGo (L [ KW "alias" , i "UserID" , tp "string" ])
       `shouldPrint`
       "type UserID string"

    it "two-var function result" $
      printGo (L [ KW "set" , i "x" , i "ok" , L [ i "os.LookupEnv" , i "name" ] ])
      `shouldPrint`
      "x, ok = os.LookupEnv(name)"

    it "slice elem set" $
      printGo (L [ KW "set" , L [ KW "val" , i "names" , int 5 ] , str "Bob" ])
      `shouldPrint`
      "names[5] = \"Bob\""

    it "empty struct literal" $
      printGo (L [ tp "foo" ]) `shouldPrint` "foo{}"

    -- spellchecker:ignore SJFKD
    it "struct literal" $
      printGo (L [ tp "api" , L [ L [ i "key" , str "SJFKD" ] ] ])
      `shouldPrint`
      "api{key: \"SJFKD\"}"

    it "var from struct literal" $
      printGo (L [ KW "var" , i "x" , L [ tp "api" , L [ L
        [ i "key" , str "SJFKD" ] ] ] ])
      `shouldPrint`
      "var x = api{key: \"SJFKD\"}"

    it "func foo() {}" $
      printGo (L [ KW "func" , i "foo" , Nil , Nil ])
      `shouldPrint`
      "func foo() {}"

    it "func f(xs []string) int {...}" $
      printGo (L [ KW "func" , i "f"
        , L [ L [ i "xs" , L [ tp "slice" , tp "string" ] ] ]
        , tp "int"
        , L [ KW "return" , int 42 ] ])
      `shouldPrint`
      "func f(xs []string) int {\n  return 42\n}"

    -- spellchecker:ignore boop
    it "func foo() {...}" $
      printGo (L [ KW "func" , i "foo" , Nil , L
      [ L [ KW "var" , i "x" , int 5 ]
      , L [ i "boop" , i "x" ] ] ])
      `shouldPrint`
      "func foo() {\n  var x = 5\n  boop(x)\n}"

    it "method (x bar) foo() {}" $
      printGo (L [ KW "method" , L [ i "x" , tp "bar" ] , i "foo" , Nil , Nil ])
      `shouldPrint`
      "func (x bar) foo() {}"

    it "method (bar) foo() {} #1" $
      printGo (L [ KW "method" , tp "bar" , i "foo" , Nil ])
      `shouldPrint`
      "func (bar) foo() {}"

    it "method (bar) foo() {} #2" $
      printGo (L [ KW "method" , tp "bar" , i "foo" , Nil , Nil ])
      `shouldPrint`
      "func (bar) foo() {}"

    it "func (b *bar) foo() {...}" $
      printGo (L [ KW "method"
        , L [ i "b" , L [ tp "ptr" , tp "bar" ] ]
        , i "foo" , Nil , L
          [ L [ KW "var" , i "x" , L [ op "+" , i "b" , int 3 ] ]
          , L [ i "boop" , i "x" ] ] ])
      `shouldPrint`
      "func (b *bar) foo() {\n  var x = b + 3\n  boop(x)\n}"

    it "x = string(y)" $
      printGo (L [ KW "set" , i "x" , L [ KW "cast" , tp "string" , i "y" ] ])
      `shouldPrint`
      "x = string(y)"

    it "x = []byte(y)" $
      printGo (L [ KW "set" , i "x" , L
        [ KW "cast" , L [ tp "slice" , tp "byte" ] , i "y" ] ])
      `shouldPrint`
      "x = []byte(y)"

    it "x = y.(Foo)" $
      printGo (L [ KW "set" , i "x" , L [ KW "assert" , tp "Foo" , i "y" ] ])
      `shouldPrint`
      "x = y.(Foo)"

    it "slice literal" $
      printGo (L [ L [ tp "slice" , tp "int" ] , L [ int 1 , int 2 , int 3 ] ])
      `shouldPrint`
      "[]int{1, 2, 3}"

    it "map literal" $
      printGo (L
        [ L [ tp "map" , tp "foo" , tp "int" ] , L
          [ L [ str "foo" , int 42 ]
          , L [ str "bar" , int 3 ] ] ])
      `shouldPrint`
      "map[foo]int{\"foo\": 42, \"bar\": 3}"

    it "map of slices" $
      printGo (L [ tp "map" , tp "q" , L [ tp "slice" , tp "a" ] ])
      `shouldPrint`
      "map[q][]a"

    it "map of pointers" $
      printGo (L [ tp "map" , tp "q" , L [ tp "ptr" , tp "a" ] ])
      `shouldPrint`
      "map[q]*a"

    it "map of maps" $
      printGo (L [ tp "map" , tp "x" , L [ tp "map" , tp "y" , tp "z" ] ])
      `shouldPrint`
      "map[x]map[y]z"

    it "call chain A" $
      printGo (L [ op "."
        , i "c"
        , L [ i "Echo" ]
        , L [ i "URI" , i "bot.handleSession" ] ])
      `shouldPrint`
      "c.Echo().URI(bot.handleSession)"

    it "call chain B" $
      printGo (L [ op "==" , int 0 , L
        [ op "."
        , L [ KW "val" , i "suite.t.Messages" , i "userID" ]
        , L [ i "Size" ] ] ])
      `shouldPrint`
      "0 == suite.t.Messages[userID].Size()"

    it "switch simple" $
      printGo (L [ KW "switch" , i "i" , L
        [ L [ KW "case" , int 1 , L [ i "foo" , str "one" ] ]
        , L [ KW "case" , int 2 , L [ i "bar" , str "two" ] ] ]])
      `shouldPrint`
      "switch i {\n  case 1: foo(\"one\")\n  case 2: bar(\"two\")\n}"

    it "switch no-expr default" $
      printGo (L [ KW "switch" , L
        [ L [ KW "case" , L [ op "==" , i "i" , int 1 ] , L [ i "foo" , str "one" ] ]
        , L [ KW "default" , L [ i "bar" , str "two" ] ] ]])
      `shouldPrint`
      "switch {\n  case i == 1: foo(\"one\")\n  default: bar(\"two\")\n}"

    it "switch empty case" $
      printGo (L [ KW "switch" , i "i" , L
        [ L [ KW "case" , int 0 ]
        , L [ KW "default" , L [ i "bar" , str "two" ] ] ]])
      `shouldPrint`
      "switch i {\n  case 0:\n  default: bar(\"two\")\n}"

    it "make chan" $
      printGo (L [ KW "make" , L [ tp "chan" , tp "int" ] ])
      `shouldPrint`
      "make(chan int)"

    it "bidirectional chan" $
      printGo (L [ tp "chan" , tp "int" ]) `shouldPrint` "chan int"

    -- it "send chan type" $
    --   printGo (L [ tp "<-chan" , tp "int" ]) `shouldPrint` "<-chan int"

    -- it "receive chan type" $
    --   printGo (L [ tp "chan<-" , tp "int" ]) `shouldPrint` "chan<- int"


  describe "Fun.Printer.singleLine" $ do

    it "==" $
      singleLine (L [ op "=" , i "foo" , i "bar" ]) `shouldBe`
      "(= foo bar)"

    it "var" $
      singleLine (L [ KW "var" , i "foo" , tp "int" , int 42 ]) `shouldBe`
      "(var foo :int 42)"


  describe "Fun.Desugar.desugar" $ do
    it "does nothing when there is nothing to do" $
      desugar (L [ i "foo" , i "bar" ]) `shouldBe` L [ i "foo" , i "bar" ]

    it "print" $
      desugar (L
        [ KW "package" , i "main" , L
        [ KW "func" , i "main" , L
          [ KW "print" , str "hello world" ] ]])
      `shouldBe` L
        [ KW "package", i "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , i "main" , L
          [ i "fmt.Println" , str "hello world" ] ]]

    it "printf" $
      desugar (L
        [ KW "package" , i "main" , L
        [ KW "func" , i "main" , L
          [ KW "printf" , str "<%s>\\n" , str "hello world" ] ]])
      `shouldBe` L
        [ KW "package", i "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , i "main" , L
          [ i "fmt.Printf" , str "<%s>\\n" , str "hello world" ] ]]

    it "print with existing import" $
      desugar (L
        [ KW "package" , i "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , i "main" , L
          [ KW "print" , str "hello world" ] ]])
      `shouldBe` L
        [ KW "package" , i "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , i "main" , L
          [ i "fmt.Println" , str "hello world" ] ]]

    it "deeper nested print" $
      desugar (L
        [ KW "package" , i "main" , L
        [ KW "func" , i "main" , L
          [ KW "for" , L
            [ KW "print" , str "hello world" ] ]]])
      `shouldBe` L
        [ KW "package", i "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , i "main" , L
          [ KW "for" , L
            [ i "fmt.Println" , str "hello world" ] ]]]

    it "print and printf" $
      desugar (L
        [ KW "package" , i "main" , L
        [ KW "func" , i "main" , L
          [ L [ KW "print" , str "<first>" ]
          , L [ KW "printf"
              , str "<%s>\\n"
              , str "second" ] ]]])
      `shouldBe` L
        [ KW "package", i "main" , L
        [ KW "import" , str "fmt" ] , L
        [ KW "func" , i "main" , L
          [ L [ i "fmt.Println" , str "<first>" ]
          , L [ i "fmt.Printf"
              , str "<%s>\\n"
              , str "second" ] ]]]

    it "adds return to constant func" $
      desugar (L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "Version" , Nil , tp "string" ,
          str "v1.02" ] ])
      `shouldBe` L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "Version" , Nil , tp "string" ,
          L [ KW "return" , str "v1.02" ] ]]

    it "adds return to func with declared results" $
      desugar (L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "double" , L [ L [ i "x" , tp "int" ] ] , tp "int" ,
          L [ op "*" , i "x" , int 2 ] ]])
      `shouldBe` L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "double" , L [ L [ i "x" , tp "int" ] ] , tp "int" ,
          L [ KW "return" , L [ op "*" , i "x" , int 2 ] ] ]]

    it "adds return to the last expression" $
      desugar (L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "double" , L [ L [ i "x" , tp "int" ] ] , tp "int" , L
          [ L [ KW "var" , i "y" , L [ op "*" , i "x" , int 2 ] ]
          , i "y" ]]])
      `shouldBe` L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "double" , L [ L [ i "x" , tp "int" ] ] , tp "int" , L
          [ L [ KW "var" , i "y" , L [ op "*" , i "x" , int 2 ] ]
          , L [ KW "return" , i "y" ] ]]]

    it "does not add return if already present" $
      desugar (L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "double" , L [ L [ i "x" , tp "int" ] ] , tp "int" ,
          L [ KW "return" , L [ op "*" , i "x" , int 2 ] ] ]])
      `shouldBe` L
        [ KW "package" , i "acme" , L
        [ KW "func" , i "double" , L [ L [ i "x" , tp "int" ] ] , tp "int" ,
          L [ KW "return" , L [ op "*" , i "x" , int 2 ] ] ]]


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
    compare (L [i "foo"]) (i "bar") `shouldBe` Ord.GT

  it "compare A L" $
    compare (i "foo") (L [i "bar"]) `shouldBe` Ord.LT
