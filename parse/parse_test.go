package parse_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/parse"
)

func TestPackage(t *testing.T) {
	tree := &fun.Package{
		Name: "Main",
		Imports: []fun.Import{
			fun.Import{Path: "fmt"},
			fun.Import{Path: "io", Alias: "io"},
		},
	}
	ast, err := parse.Package(ex(fullSource))
	if assert.NoError(t, err) {
		assert.Equal(t, tree, ast)
	}
}

func TestPackage_not_a_package(t *testing.T) {
	_, err := parse.Package("module     Test  whe\n")
	assert.EqualError(t, err, "found 'm' expected \"package\" at Ln 1, Col 1")
}

func TestPackage_brokenImport(t *testing.T) {
	_, err := parse.Package("module Test where\nimport \"fff\n")
	assert.EqualError(t, err, "found '\n' expected '\"' at Ln 2, Col 12")
}

func ex(source string) string {
	lines := strings.Split(strings.TrimSpace(source), "\n")
	for i := 0; i < len(lines); i++ {
		lines[i] = strings.TrimPrefix(lines[i], "\t")
	}
	return strings.Join(lines, "\n")
}

const fullSource = `
module Main where

import "fmt"
import "io" as "io"

inc :: int -> int
inc val = val + 1

print42 :: IO ()
print42 = fmt.Println 42

main :: IO ()
main = do
	line := "Hello World!"
	fmt.Fprintln(io.Discard, line)
`

func TestParse_package_noErr(t *testing.T) {
	src := ex(`
	package main
	
	func main = print "hello world"
	`)
	ast := fun.Package{Name: "main"}
	result, err := parse.Package(src)
	if assert.NoError(t, err) {
		assert.Equal(t, ast, *result)
	}
}
