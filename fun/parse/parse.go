// Package parse contains convinience wrapper arround Fun lexer and parser.
package parse

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"reflect"
	"strings"

	"github.com/jBugman/fun-lang/fun"
	"github.com/mitchellh/mapstructure"
)

// Package parses Package using Haskell parser.
func Package(source []byte) (*fun.Package, error) {
	var err error
	// Use func-parse tool to parse source to JSON
	encoded, err := Offload([]byte(source))
	if err != nil {
		return nil, err
	}
	// Parse JSON to a map
	var raw map[string]interface{}
	if err = json.Unmarshal(encoded, &raw); err != nil {
		return nil, err
	}
	// Convert map to a typed data via reflexion
	var result fun.Package
	if err = decodeMap(raw, &result); err != nil {
		return nil, err
	}
	return &result, nil
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

const (
	typeField = "$type"
	dataField = "$data"
)

// unwrapStruct converts JSON value to a concrete struct using type metadata.
func unwrapStruct(src reflect.Type, target reflect.Type, data interface{}) (interface{}, error) {
	if src != reflect.TypeOf(map[string]interface{}(nil)) {
		return data, nil
	}
	m := data.(map[string]interface{})

	var ok bool
	var t interface{}
	if t, ok = m[typeField]; !ok {
		return data, nil // returnining non-structs
	}

	var err error
	var payload interface{}

	if payload, ok = m[dataField]; !ok {
		return nil, fmt.Errorf("missing field \"%s\" in %v", dataField, m)
	}

	switch t {
	case "Package":
		var result fun.Package
		if err = decodeMap(payload, &result); err != nil {
			return nil, err
		}
		if len(result.Imports) == 0 {
			result.Imports = nil
		}
		return result, nil

	case "FuncDecl":
		var result fun.FuncDecl
		if err = decodeMap(payload, &result); err != nil {
			return nil, err
		}
		if len(result.Params) == 0 {
			result.Params = nil
		}
		if len(result.Results) == 0 {
			result.Results = nil
		}
		return result, nil

	case "Inline":
		var result fun.Inline
		if err = decodeMap(payload, &result); err != nil {
			return nil, err
		}
		return result, nil

	case "Single":
		var result fun.Single
		if err = decodeMap(payload, &result); err != nil {
			return nil, err
		}
		return result, nil

	case "FuncName":
		var result fun.FuncName
		if err = decodeMap(payload, &result); err != nil {
			return nil, err
		}
		return result, nil

	case "Application":
		var result fun.Application
		if err = decodeMap(payload, &result); err != nil {
			return nil, err
		}
		if len(result.Args) == 0 {
			result.Args = nil
		}
		return result, nil

	case "StringLit":
		var result fun.StringLit
		if err = decodeMap(payload, &result); err != nil {
			return nil, err
		}
		return result, nil

	case "Import":
		return payload, nil

	default:
		return nil, fmt.Errorf("type is not supported: %s", t)
	}
}

func decodeMap(data, result interface{}) error {
	var err error
	config := &mapstructure.DecoderConfig{
		DecodeHook: unwrapStruct,
		Result:     result,
	}
	var decoder *mapstructure.Decoder
	if decoder, err = mapstructure.NewDecoder(config); err != nil {
		return err
	}
	if err = decoder.Decode(data); err != nil {
		fmt.Fprintln(os.Stderr, "target:", reflect.TypeOf(result))
		fmt.Fprintln(os.Stderr, "data:", data)
		fmt.Fprintln(os.Stderr)
		return err
	}
	return nil
}
