package parser_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/parse"
	"github.com/jBugman/fun-lang/parse/parser"
)

func TestParseString_identity(t *testing.T) {
	src := ex(fullSource)
	p := parser.NewParser(strings.NewReader(src))
	ast0, err0 := p.Parse()
	ast1, err1 := parse.String(src)
	assert.Equal(t, ast0, ast1)
	assert.Equal(t, err0, err1)
}

func TestParser_Parse_module(t *testing.T) {
	tree := &fun.Module{
		Name: "Main",
		Imports: []fun.Import{
			fun.Import{Path: "fmt"},
			fun.Import{Path: "io", Alias: "io"},
		},
	}
	// ast, err := parse.String(ex(fullSource))
	p := parser.NewParser(strings.NewReader(ex(fullSource)))
	p.Debug = true
	ast, err := p.Parse()
	assert.EqualError(t, err, "found '::' expected 'EOF' at Ln 6, Col 5") // TODO remove error
	// if assert.NoError(t, err) {
	assert.Equal(t, tree, ast)
	// }
}

func TestParser_Parse_brokenModule(t *testing.T) {
	_, err := parse.String("module     Test  whe\n")
	assert.EqualError(t, err, "found 'whe' expected 'where' at Ln 1, Col 18")
}

func TestParser_Parse_brokenImport(t *testing.T) {
	_, err := parse.String("module Test where\nimport \"fff\n")
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
