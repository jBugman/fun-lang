// Package parse contains convinience wrapper arround Fun lexer and parser.
package parse

import (
	"bytes"
	"encoding/json"
	"errors"
	"os/exec"
	"strings"

	"github.com/jBugman/fun-lang/fun"
)

// Package parses string as a Package using Haskell parser.
func Package(source string) (*fun.Package, error) {
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
		msg := strings.TrimSuffix(eb.String(), "\n")
		return nil, errors.New(msg)
	default:
		return nil, err
	}
}

func decodeJSON(source []byte) (*fun.Package, error) {
	var p fun.Package
	err := json.Unmarshal(source, &p)
	if err != nil {
		return nil, err
	}
	return &p, nil
}
