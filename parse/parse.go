// Package parse contains convinience wrapper arround Fun lexer and parser.
package parse

import (
	"bytes"
	"strings"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/parse/parser"
)

// String is a shortcut to string parsing.
func String(source string) (*fun.Module, error) {
	p := parser.NewParser(strings.NewReader(source))
	return p.Parse()
}

// Bytes is a shortcut to []byte parsing.
func Bytes(source []byte) (*fun.Module, error) {
	p := parser.NewParser(bytes.NewReader(source))
	return p.Parse()
}
