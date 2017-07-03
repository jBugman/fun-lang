package parse_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/fun/parse"
)

func TestPackage_inline_main(t *testing.T) {
	src := ex(`
	package main

	import "fmt"
	import "io" as "myIO"

	func main = inline
	    line := "Hello World!"
	    fmt.Fprintln(myIO.Discard, line)
	`)
	tree := &fun.Package{
		Name: "main",
		Imports: []fun.Import{
			fun.Import{Path: "fmt"},
			fun.Import{Path: "io", Alias: "myIO"},
		},
		TopLevels: []fun.TopLevel{
			fun.FuncDecl{
				Name: "main",
				Body: fun.Inline{Block: []string{
					`line := "Hello World!"`,
					`fmt.Fprintln(myIO.Discard, line)`,
				}}}}}
	ast, err := parse.Package([]byte(src))
	if assert.NoError(t, err) {
		assert.Equal(t, tree, ast)
	}
}

func TestPackage_not_a_package(t *testing.T) {
	const src = "module     Test  whe\n"
	_, err := parse.Package([]byte(src))
	assert.EqualError(t, err, "found 'm' expected \"package\" at Ln 1, Col 1")
}

func TestPackage_brokenImport(t *testing.T) {
	const src = "package test\nimport \"fff\n"
	_, err := parse.Package([]byte(src))
	assert.Error(t, err)
}

func TestPackage_helloWorld(t *testing.T) {
	src := ex(`
	package main
	
	func main = print "hello world"
	`)
	ast := fun.Package{
		Name: "main",
		TopLevels: []fun.TopLevel{
			fun.FuncDecl{
				Name: "main",
				Body: fun.Single{
					Expr: fun.Application{
						Fun:  fun.Selector{X: "print"},
						Args: []fun.Expr{fun.StringLit{V: "hello world"}},
					}}}}}
	result, err := parse.Package([]byte(src))
	if assert.NoError(t, err) {
		assert.Equal(t, ast, *result)
	}
}

func TestPackage_func_params(t *testing.T) {
	src := ex(`
	package pkg
	
	func foo (x int) = 42
	`)
	ast := fun.Package{
		Name: "pkg",
		TopLevels: []fun.TopLevel{
			fun.FuncDecl{
				Name:   "foo",
				Params: []fun.Param{fun.NewParam("x", "int")},
				Body: fun.Single{
					Expr: fun.IntegerLit{V: 42},
				}}}}
	/// TODO: add BinaryOp to parser
	// Expr: fun.BinaryOp{
	// 	X:  fun.IntegerLit{V: 42},
	// 	Op: fun.Operator("+"),
	// 	Y:  fun.Var("x"),
	// }}}}}
	result, err := parse.Package([]byte(src))
	if assert.NoError(t, err) {
		assert.Equal(t, ast, *result)
	}
}

func ex(source string) string {
	lines := strings.Split(strings.TrimSpace(source), "\n")
	for i := 0; i < len(lines); i++ {
		lines[i] = strings.TrimPrefix(lines[i], "\t")
	}
	return strings.Join(lines, "\n")
}
