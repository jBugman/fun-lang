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
	// Unfortunately, it won't work via reflection, so there must be a lot of duplicated code.

	case "Package":
		var result fun.Package
		err = decodeMap(payload, &result)
		return result, err

	case "Import":
		var result fun.Import
		err = decodeMap(payload, &result)
		return result, err

	case "FuncDecl":
		var result fun.FuncDecl
		err = decodeMap(payload, &result)
		return result, err

	case "Atomic":
		var result fun.Atomic
		err = decodeMap(payload, &result)
		return result, err

	case "Slice":
		var result fun.Slice
		err = decodeMap(payload, &result)
		return result, err

	case "Map":
		var result fun.Map
		err = decodeMap(payload, &result)
		return result, err

	case "VarSpec":
		var result fun.VarSpec
		err = decodeMap(payload, &result)
		return result, err

	case "Param":
		var result fun.Param
		err = decodeMap(payload, &result)
		return result, err

	case "Undefined":
		var result fun.Undefined
		err = decodeMap(payload, &result)
		return result, err

	case "Single":
		var result fun.Single
		err = decodeMap(payload, &result)
		return result, err

	case "Inline":
		var result fun.Inline
		err = decodeMap(payload, &result)
		return result, err

	case "FuncName":
		var result fun.FuncName
		err = decodeMap(payload, &result)
		return result, err

	case "Application":
		var result fun.Application
		err = decodeMap(payload, &result)
		return result, err

	case "BinaryOp":
		var result fun.BinaryOp
		err = decodeMap(payload, &result)
		return result, err

	case "Operator":
		var result fun.Operator
		err = decodeMap(payload, &result)
		return result, err

	case "StringLit":
		var result fun.StringLit
		err = decodeMap(payload, &result)
		return result, err

	case "CharLit":
		var result fun.CharLit
		err = decodeMap(payload, &result)
		return result, err

	case "IntegerLit":
		var result fun.IntegerLit
		err = decodeMap(payload, &result)
		return result, err

	case "DoubleLit":
		var result fun.DoubleLit
		err = decodeMap(payload, &result)
		return result, err

	case "BoolLit":
		var result fun.BoolLit
		err = decodeMap(payload, &result)
		return result, err

	case "HexLit":
		var result fun.HexLit
		err = decodeMap(payload, &result)
		return result, err

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
	// Set empty slices in the struct fields to nil.
	v := reflect.ValueOf(result).Elem()
	if v.Kind() != reflect.Struct {
		return nil
	}
	for i := 0; i < v.NumField(); i++ {
		fld := v.Field(i)
		if fld.Kind() == reflect.Slice && fld.Len() == 0 {
			fld.Set(reflect.Zero(fld.Type()))
		}
	}
	return nil
}
