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
		fmt.Print(string(source))
		return
	}
	result, err := print.FixFormat(source)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// package main
	//
	// import "fmt"
	//
	// func main() {
	// 	fmt.Println("Hello World!")
	// }
}
