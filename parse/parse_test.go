package parse_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/parse"
)

func TestPackage_inline_main(t *testing.T) {
	src := ex(`
	package main

	import "fmt"
	import "io" as "io"

	func main = inline
	    line := "Hello World!"
	    fmt.Fprintln(io.Discard, line)
	`)
	tree := &fun.Package{
		Name: "Main",
		Imports: []fun.Import{
			fun.Import{Path: "fmt"},
			fun.Import{Path: "io", Alias: "io"},
		},
		TopLevels: []fun.TopLevel{
			fun.FuncDecl{
				Name: "main",
				Body: fun.Inline{Block: []string{
					`line := "Hello World!"`,
					`fmt.Fprintln(io.Discard, line)`,
				}}}}}
	ast, err := parse.Package(src)
	if assert.NoError(t, err) {
		assert.Equal(t, tree, ast)
	}
}

func TestPackage_not_a_package(t *testing.T) {
	_, err := parse.Package("module     Test  whe\n")
	assert.EqualError(t, err, "found 'm' expected \"package\" at Ln 1, Col 1")
}

func TestPackage_brokenImport(t *testing.T) {
	_, err := parse.Package("package test\nimport \"fff\n")
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
				Body: fun.SingleExprBody{
					Expr: fun.FuncApplication{
						Func:      fun.FunctionVal{Name: "print"},
						Arguments: []fun.Expression{fun.String("hello world")},
					}}}}}
	result, err := parse.Package(src)
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
