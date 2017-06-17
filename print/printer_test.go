package print_test

import (
	"fmt"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/print"
)

func ExampleFixFormat() {
	src := `func f ( 
		
	)   {  return 1   + 2
		}
		`
	result, err := print.FixFormat([]byte(src))
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// func f() {
	// 	return 1 + 2
	// }
}

func gofmt(src interface{}) {
	var source []byte
	switch s := src.(type) {
	case string:
		source = []byte(s)
	case []byte:
		source = s
	}
	result, err := print.FixFormat(source)
	if err != nil {
		fmt.Print(err)
		fmt.Print(string(source))
	} else {
		fmt.Print(result)
	}
}

func ExampleModule() {
	tree := fun.Module{
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
						Arguments: []fun.Expression{
							fun.String("Hello World!"),
						},
					},
				},
			},
		},
	}
	source, err := print.Module(tree)
	if err != nil {
		fmt.Println(err)
		return
	}
	gofmt(source)
	// Output:
	// package main
	//
	// import "fmt"
	//
	// func main() {
	// 	fmt.Println("Hello World!")
	// }
}

func ExampleModule_multiImports() {
	tree := fun.Module{
		Name: "Test",
		Imports: []fun.Import{
			{Path: "log"},
			{Path: "./log", Alias: "myLog"},
		},
	}
	source, err := print.Module(tree)
	if err != nil {
		fmt.Println(err)
		return
	}
	gofmt(source)
	// Output:
	// package test
	//
	// import (
	//	myLog "./log"
	//	"log"
	// )
}

func ExampleFuncDecl_infixReturn() {
	tree := fun.FuncDecl{
		Name:    "myFunc",
		Results: fun.SingleResult(fun.AtomicType("int")),
		Body: fun.SingleExprBody{
			Expr: fun.InfixOperation{
				X:        fun.Val("x"),
				Operator: fun.Operator("+"),
				Y:        fun.Int("2"),
			}},
	}
	source, err := print.FuncDecl(tree)
	if err != nil {
		fmt.Println(err)
		return
	}
	gofmt(source)
	// Output:
	// func myFunc() int {
	//	return x + 2
	// }
}

func ExampleFuncDecl_doBlock_multiline() {
	tree := fun.FuncDecl{
		Name:   "printHash",
		Params: fun.Parameters{fun.NewParam("str", "string")},
		Body: fun.DoBlock{Text: []string{
			`h := md5.New()`,
			`io.WriteString(h, str)`,
			`fmt.Printf("%x", h.Sum(nil))`,
		}},
	}
	source, err := print.FuncDecl(tree)
	if err != nil {
		fmt.Println(err)
		return
	}
	gofmt(source)
	// Output:
	// func printHash(str string) {
	// 	h := md5.New()
	// 	io.WriteString(h, str)
	// 	fmt.Printf("%x", h.Sum(nil))
	// }
}

func ExampleFuncDecl_multiReturn() {
	tree := fun.FuncDecl{
		Name:   "swap",
		Params: fun.Parameters{fun.NewParam("x", "int"), fun.NewParam("y", "int")},
		Results: fun.Results{
			Pure:  true,
			Types: []fun.Type{fun.AtomicType("int"), fun.AtomicType("int")},
		},
		Body: fun.SingleExprBody{
			Expr: fun.ReturnList{
				fun.Val("y"),
				fun.Val("x"),
			},
		},
	}
	source, err := print.FuncDecl(tree)
	if err != nil {
		fmt.Println(err)
		return
	}
	gofmt(source)
	// Output:
	// func swap(x int, y int) (int, int) {
	//	return y, x
	// }
}
