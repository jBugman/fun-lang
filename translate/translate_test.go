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

import (
	"fmt"
	"io"
)

func inc(val int) int {
	return val + 1
}

func print42() {
	fmt.Println(42)
}

func main() {
	line := "Hello World!"
	fmt.Fprintln(io.Discard, line)
}
`

func ExampleFun_Module() {
	fset := token.NewFileSet()
	tree, _ := parser.ParseFile(fset, "source.go", fullSource, 0)
	result, err := translate.NewFun(fset).Module(tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// module Main where
	//
	// import "fmt"
	// import "io"
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

func ExampleFun_Import() {
	fset := token.NewFileSet()
	tree := &ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: "fmt",
		},
	}
	result, err := translate.NewFun(fset).Import(tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// import "fmt"
}

func ExampleFun_Expression_selector() {
	fset := token.NewFileSet()
	tree := &ast.SelectorExpr{
		X:   &ast.Ident{Name: "fmt"},
		Sel: &ast.Ident{Name: "Println"},
	}
	result, err := translate.NewFun(fset).Expression(tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// fmt.Println
}

func ExampleFun_Expression_binary() {
	fset := token.NewFileSet()
	tree := &ast.BinaryExpr{
		X: &ast.Ident{
			Name: "val",
			Obj:  &ast.Object{Kind: ast.Var, Name: "val"},
		},
		Op: token.ADD,
		Y:  &ast.BasicLit{Kind: token.INT, Value: "1"},
	}
	result, err := translate.NewFun(fset).Expression(tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// val + 1
}

func ExampleFun_Statement_return() {
	fset := token.NewFileSet()
	tree := &ast.ReturnStmt{
		Results: []ast.Expr{
			&ast.BasicLit{Kind: token.INT, Value: "42"},
		},
	}
	result, err := translate.NewFun(fset).Statement(tree)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// 42
}

func ExampleFun_Statement_tuple() {
	fset := token.NewFileSet()
	expr, err := parser.ParseExpr("func() {return 'a', 9.99}")
	returnExpr := expr.(*ast.FuncLit).Body.List[0]
	result, err := translate.NewFun(fset).Statement(returnExpr)
	if err != nil {
		fmt.Print(err)
	} else {
		fmt.Print(result)
	}
	// Output:
	// ('a', 9.99)
}

func ExampleFun_Expression_brokenLiteral_testError() {
	fset := token.NewFileSet()
	tree := &ast.BasicLit{
		Kind:  token.BREAK,
		Value: "SNAKE!",
	}
	_, err := translate.NewFun(fset).Expression(tree)
	fmt.Print(err)
	// Output:
	// unexpected literal type:
	//      0  *ast.BasicLit {
	//      1  .  ValuePos: -
	//      2  .  Kind: break
	//      3  .  Value: "snake!"
	//      4  }
}
