{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
module Main where

import ClassyPrelude           hiding (LT)
import Data.Either.Combinators (mapLeft)
import Test.Hspec              (Spec, describe, hspec, it, shouldBe)

import qualified Data.Ord as Ord

import Foreign.Gofmt     (gofmt)
import Foreign.Parser    (parse)
import Fun               (translate)
import Fun.Desugar       (desugar)
import Fun.Errors        (Error (..), Pos (..), unError)
import Fun.Go.Printer    (printGo)
import Fun.PrettyPrinter (singleLine)
import Fun.SExpression   (pattern BL, pattern CL, pattern DL, pattern I, pattern ID, pattern INT,
                          pattern KW, pattern L, pattern LT, Literal (..), pattern Nil, pattern OP,
                          pattern SL, pattern TP)
import Test.Utils        (shouldFailOn, shouldParse, shouldPrint, translationExample)


pos1 :: Maybe Pos
pos1 = pos 1 1

pos :: Int -> Int -> Maybe Pos
pos l c = Just (Pos l c)

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

  describe "Fun.Parser.parse" $ do

    it "fails on non-singleton char lit" $
      parse `shouldFailOn` "\'foo\'"

    it "fails on empty string" $
      parse `shouldFailOn` ""

    it "fails on garbage input" $
      mapLeft unError (parse "_BANG!!")
      `shouldBe`
      Left "1:6: syntax error: expected EOF, found '!'"

    -- spellchecker: ignore nfoo
    it "ignores comments A" $
      parse "; comment\nfoo" `shouldParse` ID "foo"

    it "ignores comments B" $
      parse "; comment 1\n(foo bar)\n; comment 2" `shouldParse` L [ ID "foo", ID "bar" ]


    it "empty list" $
      parse "()" `shouldParse` Nil

    it "string lit" $
      parse "\"test\"" `shouldParse` SL "test" pos1

    it "char lit" $
      parse "\'z\'" `shouldParse` CL "z" pos1

    it "char lit" $
      parse "'\\''" `shouldParse` CL "\\'" pos1

    it "newline char" $
      parse "'\\n'" `shouldParse` CL "\\n" pos1

    it "true" $
      parse "true" `shouldParse` BL True pos1

    it "false" $
      parse "false" `shouldParse` BL False pos1

    it "int lit" $
      parse "42" `shouldParse` I 42

    it "zero" $
      parse "0" `shouldParse` I 0

    it "double lit" $
      parse "42.0" `shouldParse` DL 42.0 (pos 1 1)

    it "american double" $
      parse ".223" `shouldParse` DL 0.223 (pos 1 1)

    it "exp double lit" $
      parse "1e3" `shouldParse` DL 1000 (pos 1 1)

    it "oct lit" $
      parse "0644" `shouldParse` INT 8 0o644

    it "hex lit" $
      parse "0x2A" `shouldParse` INT 16 0x2A

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

    it "||" $
      parse "||" `shouldParse` OP "||"

    it "|" $
      parse "|" `shouldParse` OP "|"

    it "!=" $
      parse "!=" `shouldParse` OP "!="

    it "!" $
      parse "!" `shouldParse` OP "!"

    it "op + ident" $
      parse "(+ foo)" `shouldParse` L [ OP "+" , ID "foo" ]

    it "op hex int" $
      parse "(== 0xff 255)" `shouldParse` L [ OP "==" , INT 16 0xFF , INT 10 255 ]

    it "keyword" $
      parse "for" `shouldParse` KW "for"

    it "not a keyword" $
      parse "forall" `shouldParse` ID "forall"

    it "func call" $
      parse "(printf \"%+v\\n\" v)" `shouldParse`
      L [ KW "printf", SL "%+v\\n" (pos 1 9), ID "v" ]

    it "selector + unit" $
      parse "(fmt.Println ())" `shouldParse` L [ ID "fmt.Println", Nil ]

    it "ident list" $
      parse "(foo bar)" `shouldParse` L [ ID "foo", ID "bar" ]

    it "nested empty list" $
      parse "(())" `shouldParse` L [Nil]

    it "op + ident + lit" $
      parse "(< foo 10)" `shouldParse` L [ OP "<" , ID "foo" , I 10 ]

    it "import" $
      parse "(import \"foo\")" `shouldParse` L [ KW "import" , SL "foo" (pos 1 9) ]

    it "multiline s-exp" $
      parse "(+ foo bar\n    :int)" `shouldParse`
      L [ OP "+" , ID "foo" , ID "bar" , TP "int" ]

    it "multiline s-exp with a comment" $
      parse "(foo 123 456\n; comment\n  bar)" `shouldParse`
      L [ ID "foo" , I 123 , I 456 , ID "bar" ]

    it "HelloWorld" $
      parse "(package main\n\n  (func main (print \"hello world\")))" `shouldParse`
      L [ KW "package" , ID "main"
        , L [ KW "func" , ID "main"
            , L [ KW "print" , SL "hello world" (pos 3 21)] ]]

    it "var decl from slice literal" $
      parse "(var t (:slice :string) (\"g\" \"h\" \"c\"))" `shouldParse`
      L [ KW "var" , ID "t" , L [ TP "slice" , TP "string" ]
        , L [ SL "g" (pos 1 26) , SL "h" (pos 1 30) , SL "c" (pos 1 34)] ]

    it "func type" $
      parse "(:func () :int)" `shouldParse` L [ TP "func" , Nil , TP "int" ]

    it "type assertion" $
      parse "(assert :foo x)" `shouldParse` L [ KW "assert" , TP "foo" , ID "x" ]

    it "testing assert" $
      parse "(assert.Equal (s.T) text t)"
      `shouldParse`
      L [ ID "assert.Equal" , L [ ID "s.T" ] , ID "text" , ID "t" ]


  describe "Fun.Go.Printer.printGo" $ do

    it "fail on standalone Nil" $
      mapLeft unError (printGo Nil)
      `shouldBe`
      Left "translation error: not supported yet: ()"

    it "import" $
      printGo (L [ KW "import" , LT (String "fmt") ])
      `shouldPrint`
      "import \"fmt\""

    it "import with alias" $
      printGo (L [ KW "import" , LT (String "very/long-package") , LT (String "pkg") ])
      `shouldPrint`
      "import pkg \"very/long-package\""

    it "simple func" $
      printGo (L [ KW "func" , ID "setS" , L [ KW "set" , ID "s" , I 2 ] ])
      `shouldPrint`
      "func setS() {\n  s = 2\n}"

    it "<" $
      printGo (L [ OP "<" , ID "n" , I 10 ]) `shouldPrint` "n < 10"

    it "==" $
      printGo (L [ OP "==" , ID "foo" , ID "bar" ]) `shouldPrint` "foo == bar"

    it "--" $
      printGo (L [ OP "--" , ID "j" ]) `shouldPrint` "j--"

    it "* as product" $
      printGo (L [ OP "*" , ID "x" , ID "y" ]) `shouldPrint` "x * y"

    it "* as pointer" $
      printGo (L [ OP "*" , ID "foo" ]) `shouldPrint` "*foo"

    it "hex == char" $
      printGo (L
        [ OP "=="
        , INT 16 1
        , LT (Char "a")
        ]) `shouldPrint` "0x1 == 'a'"

    it "double" $
      printGo (LT (Double 9.99)) `shouldPrint` "9.99"

    it "oct" $
      printGo (INT 8 0o644) `shouldPrint` "0644"

    it "complex" $
      printGo (L [ TP "complex" , LT (Double 5.2) , I 3 ]) `shouldPrint` "5.2+3i"

    it "complex negative" $
      printGo (L [ TP "complex" , I 1 , I (-2) ]) `shouldPrint` "1-2i"

    it "string" $
      printGo (LT (String "fizzbuzz")) `shouldPrint` "\"fizzbuzz\""

    it "'any' type" $
      printGo (TP "any") `shouldPrint` "interface{}"

    it "const decl" $
      printGo (L [ KW "const" , ID "a" , LT (String "initial") ])
      `shouldPrint`
      "const a = \"initial\""

    it "full const decl" $
      printGo (L [ KW "const" , ID "a" , TP "State" , INT 16 0xA ])
      `shouldPrint`
      "const a State = 0xa"

    it "full var decl" $
      printGo (L [ KW "var" , ID "b" , TP "int" , I 1 ]) `shouldPrint` "var b int = 1"

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
      printGo (L [ KW "var" , ID "x" , ID "y" , L [ KW "val" , ID "m" , I 2 ] ])
      `shouldPrint`
      "var x, y = m[2]"

    it "var _, x = m[2]" $
      printGo (L [ KW "var" , ID "_" , ID "x" , L [ KW "val" , ID "m" , I 2 ] ])
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
        , L [ I 1 , I 2, I 3 ] ] ])
      `shouldPrint`
      "var nums = []int{1, 2, 3}"

    it "var with map literal initializer" $
      printGo (L [ KW "var" , ID "m" , L
      [ L [ TP "map" , TP "string" , TP "bool" ] , L
        [ L [ LT (String "foo"), LT (Bool True) ]
        , L [ LT (String "bar") , LT (Bool False) ] ]]])
      `shouldPrint`
      "var m = map[string]bool{\"foo\": true, \"bar\": false}"

    it "slice of slices" $
      printGo (L [ TP "slice" , L [ TP "slice" , TP "string" ] ])
      `shouldPrint`
      "[][]string"

    it "map lookup" $
      printGo (L [ KW "set" , ID "_" , ID "ok" , L [ KW "val" , ID "m" , LT (String "Bob") ] ])
      `shouldPrint`
      "_, ok = m[\"Bob\"]"

    it "slice index" $
      printGo (L [ KW "val" , ID "xs" , I 4 ]) `shouldPrint` "xs[4]"

    it "slice 2D-index" $
      printGo (L [ KW "val" , ID "xs" , ID "y" , ID "x" ]) `shouldPrint` "xs[y][x]"

    it "short range" $
      printGo (L [ KW "range" , ID "x" , ID "chanX" ]) `shouldPrint`
      "x := range chanX"

    it "full range" $
      printGo (L [ KW "range" , ID "k" , ID "v" , ID "users" ]) `shouldPrint`
      "k, v := range users"

    it "slice from" $
      printGo (L [ KW "slice" , ID "xs" , I 1 , ID "_" ]) `shouldPrint` "xs[1:]"

    it "slice to" $
      printGo (L [ KW "slice" , ID "xs" , ID "_" , I 4 ]) `shouldPrint` "xs[:4]"

    it "slice from-to" $
      printGo (L [ KW "slice" , ID "indexes" , ID "i" , L [ OP "+" , ID "j" , I 1 ] ])
      `shouldPrint` "indexes[i:j + 1]"

    it "empty struct" $
      printGo (L [ KW "struct" , ID "foo" ])
      `shouldPrint` "type foo struct{}"

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
      printGo (L [ KW "for" , L [ ID "fmt.Println" , LT (String "fizz") ] ])
      `shouldPrint`
      "for {\n  fmt.Println(\"fizz\")\n}"

    it "standard for loop" $
      printGo (L [ KW "for" , ID "i" , I 0 , I 10 , L [ ID "fmt.Println" , LT (String "buzz") ] ])
      `shouldPrint`
      "for i := 0; i < 10; i++ {\n  fmt.Println(\"buzz\")\n}"

    it "only condition for loop" $
      printGo (L [ KW "for"
        , L [ OP "<" , ID "n" , I 42 ]
        , L [ ID "fmt.Print" , LT (String "?") ] ])
      `shouldPrint`
      "for n < 42 {\n  fmt.Print(\"?\")\n}"

    it "custom for loop" $
      printGo (L [ KW "for" , ID "x" , ID "k" , LT (Bool True)
      , L [ KW "set" , ID "x" , L [ OP "+", ID "x", ID "foo.N" ] ]
      , L [ KW "break" ] ])
      `shouldPrint`
      "for x := k; true; x = x + foo.N {\n  break\n}"

    it "minimal func literal" $
      printGo (L [ KW "func" , Nil , L [ ID "pass" , I 42 ] ])
      `shouldPrint`
      "func() {\n  pass(42)\n}"

    it "func literal with result" $
      printGo (L [ KW "func" , Nil , TP "int" , L
      [ L [ KW "set" , ID "i" , L [ OP "+", ID "i", I 1 ] ]
      , L [ KW "return" , ID "i" ] ]])
      `shouldPrint`
      "func() int {\n  i = i + 1\n  return i\n}"

    it "func literal with args" $
      printGo (L [ KW "func" , L [ L [ ID "x" , TP "int" ]] , L
      [ L [ KW "set" , ID "i" , L [ OP "+", ID "i", ID "x" ] ]
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
      printGo (L [ KW "set" , L [ KW "val" , ID "names" , I 5 ] , LT (String "Bob") ])
      `shouldPrint`
      "names[5] = \"Bob\""

    it "empty struct literal" $
      printGo (L [ TP "foo" ]) `shouldPrint` "foo{}"

    -- spellchecker:ignore SJFKD
    it "struct literal" $
      printGo (L [ TP "api" , L [ L [ ID "key" , LT (String "SJFKD") ] ] ])
      `shouldPrint`
      "api{key: \"SJFKD\"}"

    it "var from struct literal" $
      printGo (L [ KW "var" , ID "x" , L [ TP "api" , L [ L
        [ ID "key" , LT (String "SJFKD") ] ] ] ])
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
        , L [ KW "return" , I 42 ] ])
      `shouldPrint`
      "func f(xs []string) int {\n  return 42\n}"

    -- spellchecker:ignore boop
    it "func foo() {...}" $
      printGo (L [ KW "func" , ID "foo" , Nil , L
      [ L [ KW "var" , ID "x" , I 5 ]
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
          [ L [ KW "var" , ID "x" , L [ OP "+" , ID "b" , I 3 ] ]
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
      printGo (L [ L [ TP "slice" , TP "int" ] , L [ I 1 , I 2 , I 3 ] ])
      `shouldPrint`
      "[]int{1, 2, 3}"

    it "map literal" $
      printGo (L
        [ L [ TP "map" , TP "foo" , TP "int" ] , L
          [ L [ LT (String "foo") , I 42 ]
          , L [ LT (String "bar") , I 3 ] ] ])
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
      printGo (L [ OP "."
        , ID "c"
        , L [ ID "Echo" ]
        , L [ ID "URI" , ID "bot.handleSession" ] ])
      `shouldPrint`
      "c.Echo().URI(bot.handleSession)"

    it "call chain B" $
      printGo (L [ OP "==" , I 0 , L
        [ OP "."
        , L [ KW "val" , ID "suite.t.Messages" , ID "userID" ]
        , L [ ID "Size" ] ] ])
      `shouldPrint`
      "0 == suite.t.Messages[userID].Size()"

    it "switch simple" $
      printGo (L [ KW "switch" , ID "i" , L
        [ L [ KW "case" , I 1 , L [ ID "foo" , LT (String "one") ] ]
        , L [ KW "case" , I 2 , L [ ID "bar" , LT (String "two") ] ] ]])
      `shouldPrint`
      "switch i {\n  case 1: foo(\"one\")\n  case 2: bar(\"two\")\n}"

    it "switch no-expr default" $
      printGo (L [ KW "switch" , L
        [ L [ KW "case" , L [ OP "==" , ID "i" , I 1 ] , L [ ID "foo" , LT (String "one") ] ]
        , L [ KW "default" , L [ ID "bar" , LT (String "two") ] ] ]])
      `shouldPrint`
      "switch {\n  case i == 1: foo(\"one\")\n  default: bar(\"two\")\n}"

    it "switch empty case" $
      printGo (L [ KW "switch" , ID "i" , L
        [ L [ KW "case" , I 0 ]
        , L [ KW "default" , L [ ID "bar" , LT (String "two") ] ] ]])
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
      singleLine (L [ OP "=" , ID "foo" , ID "bar" ]) `shouldBe`
      "(= foo bar)"

    it "var" $
      singleLine (L [ KW "var" , ID "foo" , TP "int" , I 42 ]) `shouldBe`
      "(var foo :int 42)"


  describe "Fun.Desugar.desugar" $ do
    it "does nothing when there is nothing to do" $
      desugar (L [ ID "foo" , ID "bar" ]) `shouldBe` L [ ID "foo" , ID "bar" ]

    it "print" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "print" , LT (String "hello world") ] ]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , LT (String "fmt") ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Println" , LT (String "hello world") ] ]]

    it "printf" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "printf" , LT (String "<%s>\\n") , LT (String "hello world") ] ]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , LT (String "fmt") ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Printf" , LT (String "<%s>\\n"), LT (String "hello world") ] ]]

    it "print with existing import" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "import" , LT (String "fmt") ] , L
        [ KW "func" , ID "main" , L
          [ KW "print" , LT (String "hello world") ] ]])
      `shouldBe` L
        [ KW "package" , ID "main" , L
        [ KW "import" , LT (String "fmt") ] , L
        [ KW "func" , ID "main" , L
          [ ID "fmt.Println" , LT (String "hello world") ] ]]

    it "deeper nested print" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ KW "for" , L
            [ KW "print" , LT (String "hello world") ] ]]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , LT (String "fmt") ] , L
        [ KW "func" , ID "main" , L
          [ KW "for" , L
            [ ID "fmt.Println" , LT (String "hello world") ] ]]]

    it "print and printf" $
      desugar (L
        [ KW "package" , ID "main" , L
        [ KW "func" , ID "main" , L
          [ L [ KW "print" , LT (String "<first>") ]
          , L [ KW "printf"
              , LT (String "<%s>\\n")
              , LT (String "second") ] ]]])
      `shouldBe` L
        [ KW "package", ID "main" , L
        [ KW "import" , LT (String "fmt") ] , L
        [ KW "func" , ID "main" , L
          [ L [ ID "fmt.Println" , LT (String "<first>") ]
          , L [ ID "fmt.Printf"
              , LT (String "<%s>\\n")
              , LT (String "second") ] ]]]

    it "adds return to constant func" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "Version" , Nil , TP "string" ,
          LT (String "v1.02") ] ])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "Version" , Nil , TP "string" ,
          L [ KW "return" , LT (String "v1.02") ] ]]

    it "adds return to func with declared results" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ OP "*" , ID "x" , I 2 ] ]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ OP "*" , ID "x" , I 2 ] ] ]]

    it "adds return to the last expression" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" , L
          [ L [ KW "var" , ID "y" , L [ OP "*" , ID "x" , I 2 ] ]
          , ID "y" ]]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" , L
          [ L [ KW "var" , ID "y" , L [ OP "*" , ID "x" , I 2 ] ]
          , L [ KW "return" , ID "y" ] ]]]

    it "does not add return if already present" $
      desugar (L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ OP "*" , ID "x" , I 2 ] ] ]])
      `shouldBe` L
        [ KW "package" , ID "acme" , L
        [ KW "func" , ID "double" , L [ L [ ID "x" , TP "int" ] ] , TP "int" ,
          L [ KW "return" , L [ OP "*" , ID "x" , I 2 ] ] ]]


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
