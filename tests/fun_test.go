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
			fun.FuncDecl{
				Name: "main",
				Body: fun.FuncApplication{
					Module:    "fmt",
					Name:      "Println",
					Arguments: []fun.Argument{fun.String("Hello World!")},
				},
			},
		},
	}
	fmt.Print(module)
	// Output:
	// module Main where
	//
	// import "fmt"
	//
	// main :: ()
	// main = fmt.Println "Hello World!"
}

func ExampleFuncApplication_String_local() {
	fn := fun.FuncApplication{
		Name:      "sum",
		Arguments: []fun.Argument{fun.Int(1), fun.Int(2)},
	}
	fmt.Println(fn)
	// Output:
	// sum 1 2
}

func ExampleFuncApplication_String_fmtPrintln() {
	fn := fun.FuncApplication{
		Name:      "Println",
		Module:    "fmt",
		Arguments: []fun.Argument{fun.Float(4.2)},
	}
	fmt.Println(fn)
	// Output:
	// fmt.Println 4.2
}

func ExampleFuncApplication_String_nestedFunction() {
	fn := fun.FuncApplication{
		Name: "take",
		Arguments: []fun.Argument{
			fun.Int(3),
			fun.FuncApplication{
				Name:      "reverse",
				Arguments: []fun.Argument{fun.Var("xs")},
			},
		},
	}
	fmt.Println(fn)
	// Output:
	// take 3 (reverse xs)
}

func ExampleFuncDecl_String_statementBody() {
	statement := fun.FuncApplication{
		Name:      "Println",
		Module:    "fmt",
		Arguments: []fun.Argument{fun.Var("x")},
	}
	fn := fun.FuncDecl{
		Name:   "printInt",
		Params: fun.Parameters{{"x", "int"}},
		Body:   statement,
	}
	fmt.Println(fn)
	// Output:
	// printInt :: int -> ()
	// printInt x = fmt.Println x
}

func ExampleDoBlock_String_oneLine() {
	fn := fun.DoBlock{[]string{`fmt.Fprintf(&b, "world!")`}}
	fmt.Println(fn)
	// Output:
	// do
	//     fmt.Fprintf(&b, "world!")
}

func ExampleFuncDecl_String_doBlock_multiline() {
	fn := fun.FuncDecl{
		Name:   "printHash",
		Params: fun.Parameters{{"str", "string"}},
		Body: fun.DoBlock{[]string{
			`h := md5.New()`,
			`io.WriteString(h, str)`,
			`fmt.Printf("%x", h.Sum(nil))`,
		}},
	}
	fmt.Println(fn)
	// Output:
	// printHash :: string -> ()
	// printHash str = do
	//     h := md5.New()
	//     io.WriteString(h, str)
	//     fmt.Printf("%x", h.Sum(nil))
}
