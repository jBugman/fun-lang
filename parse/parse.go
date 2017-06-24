// Package parse contains convinience wrapper arround Fun lexer and parser.
package parse

import (
	"bytes"
	"os/exec"
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

// Offload uses func-parse tool to parse
func Offload(source []byte) ([]byte, error) {
	const path = ".stack-work/install/x86_64-osx/lts-8.20/8.0.2/bin/" // temporary dev path
	cmd := exec.Command(path + "func-parse")
	cmd.Stdin = bytes.NewReader(source)
	out, err := cmd.CombinedOutput()
	return out, err
}
