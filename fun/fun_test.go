package fun_test

import (
	"fmt"

	"github.com/jBugman/fun-lang/fun"
)

func ExampleImport_String_single() {
	fmt.Println(fun.Import{Path: "github.com/awesome/lib/fmt", Alias: "awfmt"})
	// Output:
	// import "github.com/awesome/lib/fmt" as "awfmt"
}

func ExampleParameters_String_single() {
	fmt.Println(fun.Parameters{{Name: "s", Type: fun.AtomicType("string")}})
	// Output:
	// string
}

func ExampleParameters_String_two() {
	fmt.Println(fun.Parameters{fun.NewParam("x", "int"), fun.NewParam("y", "int")})
	// Output:
	// int -> int
}

func ExampleParameters_String_zero() {
	fmt.Println(fun.Parameters{})
	// Output:
	//
}

func ExampleResults_String_one() {
	fmt.Println(fun.Results{fun.ObjectType("io.Writer")})
	// Output:
	// io.Writer
}

func ExampleResults_String_two() {
	fmt.Println(fun.Results{fun.AtomicType("int"), fun.AtomicType("error")})
	// Output:
	// (int, error)
}
func ExampleResults_String_zero() {
	fmt.Println(fun.Results{})
	// Output:
	// ()
}

func ExampleFuncDecl_String_unit_implicit_undefined() {
	fn := fun.FuncDecl{Name: "main"}
	fmt.Println(fn)
	// Output:
	// main :: ()
	// main = undefined
}

func ExampleFuncDecl_String_unit_explicit_undefined() {
	fn := fun.FuncDecl{
		Name:   "newFunc",
		Params: fun.Parameters{fun.NewParam("launchMissles", "bool")},
		Body:   fun.Undefined,
	}
	fmt.Println(fn)
	// Output:
	// newFunc :: bool -> ()
	// newFunc launchMissles = undefined
}

func ExampleFuncDecl_String_int_unit_undefined() {
	fn := fun.FuncDecl{
		Name:   "print",
		Params: fun.Parameters{{Name: "n", Type: fun.AtomicType("int")}},
	}
	fmt.Println(fn)
	// Output:
	// print :: int -> ()
	// print n = undefined
}

func ExampleFuncDecl_String_head_undefined() {
	fn := fun.FuncDecl{
		Name:   "head",
		Params: fun.Parameters{{Name: "xs", Type: fun.NewList("int")}},
		Results: fun.Results{
			fun.AtomicType("int"),
			fun.AtomicType("error"),
		},
	}
	fmt.Println(fn)
	// Output:
	// head :: [int] -> (int, error)
	// head xs = undefined
}

func ExampleFuncDecl_String_params() {
	fn := fun.FuncDecl{
		Name:    "enumFromTo",
		Params:  fun.Parameters{fun.NewParam("x", "a"), fun.NewParam("y", "a")},
		Results: fun.Results{fun.NewList("a")},
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
				Body: fun.SingleExprBody{
					Expr: fun.FuncApplication{
						Func: fun.FunctionVal{
							Module: "fmt",
							Name:   "Println",
						},
						Arguments: []fun.Expression{fun.String("Hello World!")},
					},
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
		Func:      fun.FunctionVal{Name: "sum"},
		Arguments: []fun.Expression{fun.Int("1"), fun.Int("2")},
	}
	fmt.Println(fn)
	// Output:
	// sum 1 2
}

func ExampleFuncApplication_String_fmtPrintln() {
	fn := fun.FuncApplication{
		Func: fun.FunctionVal{
			Name:   "Println",
			Module: "fmt",
		},
		Arguments: []fun.Expression{fun.Float("4.2")},
	}
	fmt.Println(fn)
	// Output:
	// fmt.Println 4.2
}

func ExampleFuncApplication_String_nestedFunction() {
	fn := fun.FuncApplication{
		Func: fun.FunctionVal{Name: "take"},
		Arguments: []fun.Expression{
			fun.Int("3"),
			fun.FuncApplication{
				Func:      fun.FunctionVal{Name: "reverse"},
				Arguments: []fun.Expression{fun.Val("xs")},
			},
		},
	}
	fmt.Println(fn)
	// Output:
	// take 3 (reverse xs)
}

func ExampleFuncDecl_String_singleExprBody() {
	expr := fun.FuncApplication{
		Func: fun.FunctionVal{
			Name:   "Println",
			Module: "fmt",
		},
		Arguments: []fun.Expression{fun.Val("x")},
	}
	fn := fun.FuncDecl{
		Name:   "printInt",
		Params: fun.Parameters{fun.NewParam("x", "int")},
		Body:   fun.SingleExprBody{Expr: expr},
	}
	fmt.Println(fn)
	// Output:
	// printInt :: int -> ()
	// printInt x = fmt.Println x
}

func ExampleDoBlock_String_oneLine() {
	fn := fun.DoBlock{Text: []string{`fmt.Fprintf(&b, "world!")`}}
	fmt.Println(fn)
	// Output:
	// do
	//     fmt.Fprintf(&b, "world!")
}

func ExampleFuncDecl_String_doBlock_multiline() {
	fn := fun.FuncDecl{
		Name:   "printHash",
		Params: fun.Parameters{fun.NewParam("str", "string")},
		Body: fun.DoBlock{Text: []string{
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

func ExampleInfixOperation_String() {
	op := fun.InfixOperation{
		X:        fun.Val("x"),
		Operator: fun.Operator("+"),
		Y:        fun.Int("2"),
	}
	fmt.Println(op)
	// Output:
	// x + 2
}

func ExampleTuple_String() {
	t := fun.Tuple{
		fun.Val("operator"),
		fun.String("plus"),
	}
	fmt.Println(t)
	// Output:
	// (operator, "plus")
}
