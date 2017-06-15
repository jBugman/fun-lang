package tests

import (
	"fmt"
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
