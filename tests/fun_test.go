package tests

import (
	"fmt"

	"../fun"
)

func ExampleParameters_String_single() {
	fmt.Println(fun.Parameters{{Name: "s", Type: "string"}})
	// Output:
	// string
}

func ExampleParameters_String_two() {
	fmt.Println(fun.Parameters{{"x", "int"}, {"y", "int"}})
	// Output:
	// int -> int
}

func ExampleParameters_String_zero() {
	fmt.Println(fun.Parameters{})
	// Output:
	//
}

func ExampleResults_String_one() {
	fmt.Println(fun.Results{"io.Writer"})
	// Output:
	// io.Writer
}

func ExampleResults_String_two() {
	fmt.Println(fun.Results{"int", "error"})
	// Output:
	// (int, error)
}
func ExampleResults_String_zero() {
	fmt.Println(fun.Results{})
	// Output:
	// ()
}

func ExampleFuncDecl_String_unit_undefined() {
	fn := fun.FuncDecl{Name: "main"}
	fmt.Println(fn)
	// Output:
	// main :: ()
	// main = undefined
}

func ExampleFuncDecl_String_int_unit_undefined() {
	fn := fun.FuncDecl{
		Name:   "print",
		Params: fun.Parameters{{"n", "int"}},
	}
	fmt.Println(fn)
	// Output:
	// print :: int -> ()
	// print n = undefined
}

func ExampleFuncDecl_String_head_undefined() {
	fn := fun.FuncDecl{
		Name:    "head",
		Params:  fun.Parameters{{"xs", "[int]"}},
		Results: fun.Results{"int", "error"},
	}
	fmt.Println(fn)
	// Output:
	// head :: [int] -> (int, error)
	// head xs = undefined
}

func ExampleFuncDecl_String_params() {
	fn := fun.FuncDecl{
		Name:    "enumFromTo",
		Params:  fun.Parameters{{"x", "a"}, {"y", "a"}},
		Results: fun.Results{"[a]"},
	}
	fmt.Println(fn)
	// Output:
	// enumFromTo :: a -> a -> [a]
	// enumFromTo x y = undefined
}

func ExampleCreateAST() {
	module := fun.Module{
		Name: "Main",
		Imports: []fun.Import{
			{Path: "fmt"},
		},
		Decls: []fun.Decl{
			fun.FuncDecl{Name: "main"},
		},
	}
	fmt.Print(module)
	// Output:
	// module Main where
	//
	// import "fmt"
	//
	// main :: ()
	// main = undefined
}
