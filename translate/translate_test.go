package translate_test

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"

	"../translate"
)

const fullSource = `
package main

import "fmt"
import "io/ioutil"

func inc(val int) int {
	return val + 1
}

func print42() {
	fmt.Println(42)
}

func main() {
	line := "Hello World!"
	fmt.Fprintln(ioutil.Discard, line)
}
`

func ExampleFromFile() {
	fset := token.NewFileSet()
	goTree, _ := parser.ParseFile(fset, "source.go", fullSource, 0)
	module, _ := translate.FromFile(fset, goTree)
	fmt.Print(module)
	// Output:
	// module Main where
	//
	// import "fmt"
	// import "io/ioutil"
	//
	// inc :: int -> int
	// inc val = val + 1
	//
	// print42 :: ()
	// print42 = fmt.Println 42
	//
	// main :: ()
	// main = do
	//     line := "Hello World!"
	//     fmt.Fprintln(io.Discard, line)
}

func ExampleImport() {
	// fset := token.NewFileSet()
	tree := ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: "fmt",
		},
	}
	imp, _ := translate.Import(&tree)
	fmt.Print(imp)
	// Output:
	// import "fmt"
}

func ExampleExpression_selector() {
	fset := token.NewFileSet()
	tree := ast.SelectorExpr{
		X:   &ast.Ident{Name: "fmt"},
		Sel: &ast.Ident{Name: "Println"},
	}
	expr, err := translate.Expression(fset, &tree)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Print(expr)
	}
	// Output:
	// fmt.Println
}

func ExampleExpression_binary() {
	fset := token.NewFileSet()
	tree := ast.BinaryExpr{
		X: &ast.Ident{
			Name: "val",
			Obj:  &ast.Object{Kind: ast.Var, Name: "val"},
		},
		Op: token.ADD,
		Y:  &ast.BasicLit{Kind: token.INT, Value: "1"},
	}
	expr, err := translate.Expression(fset, &tree)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Print(expr)
	}
	// Output:
	// val + 1
}
