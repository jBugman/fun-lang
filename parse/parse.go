// Package parse contains convinience wrapper arround Fun lexer and parser.
package parse

import (
	"bytes"
	"encoding/json"
	"os/exec"

	"errors"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/parse/parser"
)

// String is a shortcut to string parsing. Uses Haskell parser.
func String(source string) (*fun.Module, error) {
	jsonString, err := Offload([]byte(source))
	if err != nil {
		return nil, err
	}
	p, err := decodeJSON(jsonString)
	if err != nil {
		return nil, err
	}
	return p, nil
}

// Bytes is a shortcut to []byte parsing. Uses Go parser.
func Bytes(source []byte) (*fun.Module, error) {
	p := parser.NewParser(bytes.NewReader(source))
	return p.Parse()
}

// Offload uses func-parse tool to parse
func Offload(source []byte) ([]byte, error) {
	cmd := exec.Command("func-parse")
	cmd.Stdin = bytes.NewReader(source)
	var ob bytes.Buffer
	var eb bytes.Buffer
	cmd.Stdout = &ob
	cmd.Stderr = &eb
	err := cmd.Run()
	switch err.(type) {
	case nil:
		return ob.Bytes(), nil
	case *exec.ExitError:
		return nil, errors.New(eb.String())
	default:
		return nil, err
	}
}

func decodeJSON(source []byte) (*fun.Module, error) {
	var p fun.Module
	err := json.Unmarshal(source, &p)
	if err != nil {
		return nil, err
	}
	return &p, nil
}
