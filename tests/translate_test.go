package tests

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"

	"../translate"
)

const source = `
package main

import "fmt"
import "io/ioutil"

func inc(val int) int {
	return val + 1
}

func main() {
	fmt.Fprintln(ioutil.Discard, "Hello World!")
}
`

func parseTree(src string) *ast.File {
	fset := token.NewFileSet()
	tree, _ := parser.ParseFile(fset, "source.go", src, 0)
	return tree
}

func ExampleFromAST() {
	golangTree := parseTree(source)
	translated := translate.FromAST(golangTree)
	fmt.Println(translated.PrettyPrint())
	// Output:
	// module Main where
	//
	// import "fmt"
	// import "io/ioutil"
	//
	// inc :: int -> int
	// inc val = val + 1
	//
	// main :: ()
	// main = fmt.Fprintln io.Discard "Hello World!"
}
