// Package parse contains convinience wrapper arround Fun lexer and parser.
package parse

import (
	"strings"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/parse/parser"
)

// String is a shortcut to string parsing.
func String(source string) (*fun.Module, error) {
	p := parser.NewParser(strings.NewReader(source))
	return p.Parse()
}
