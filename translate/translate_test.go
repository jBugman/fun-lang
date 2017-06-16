package translate_test

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"

	"github.com/jBugman/fun-lang/translate"
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
	result, err := translate.FromFile(fset, goTree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
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
	result, err := translate.Import(&tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// import "fmt"
}

func ExampleExpression_selector() {
	fset := token.NewFileSet()
	tree := ast.SelectorExpr{
		X:   &ast.Ident{Name: "fmt"},
		Sel: &ast.Ident{Name: "Println"},
	}
	result, err := translate.Expression(fset, &tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
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
	result, err := translate.Expression(fset, &tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// val + 1
}

func ExampleStatement_return() {
	fset := token.NewFileSet()
	tree := &ast.ReturnStmt{
		Results: []ast.Expr{
			&ast.BasicLit{Kind: token.INT, Value: "42"},
		},
	}
	result, err := translate.Statement(fset, tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// 42
}
