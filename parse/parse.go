// Package parse contains convinience wrapper arround Fun lexer and parser.
package parse

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
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
	obj, err := decodeJSON(jsonString)
	if err != nil {
		return nil, err
	}
	pack, err := decode(obj)
	if err != nil {
		return nil, err
	}
	return pack.(*fun.Package), nil
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

func decodeJSON(source []byte) (map[string]interface{}, error) {
	var obj map[string]interface{}
	err := json.Unmarshal(source, &obj)
	if err != nil {
		fmt.Println(string(source))
		return nil, err
	}
	return obj, nil
}

func decode(n interface{}) (interface{}, error) {
	const typeKey = "$type"
	node := n.(map[string]interface{})
	t, ok := node[typeKey]
	if !ok {
		return nil, fmt.Errorf("Missing \"%s\" key in JSON object", typeKey)
	}

	var err error
	var subnode interface{}
	switch t {
	case "package":
		var p fun.Package
		for _, obj := range node["imports"].([]interface{}) {
			subnode, err = decode(obj)
			if err != nil {
				return nil, err
			}
			p.Imports = append(p.Imports, subnode.(fun.Import))
		}
		for _, obj := range node["topDecls"].([]interface{}) {
			subnode, err = decode(obj)
			if err != nil {
				return nil, err
			}
			p.TopLevels = append(p.TopLevels, subnode.(fun.TopLevel))
		}
		return &p, nil
	case "funcDecl":
		var fd fun.FuncDecl
		fd.Name = node["name"].(string)
		for _, obj := range node["params"].([]interface{}) {
			subnode, err = decode(obj)
			if err != nil {
				return nil, err
			}
			fd.Params = append(fd.Params, subnode.(fun.Param))
		}
		for _, obj := range node["results"].([]interface{}) {
			subnode, err = decode(obj)
			if err != nil {
				return nil, err
			}
			fd.Results = append(fd.Results, subnode.(fun.Type))
		}
		subnode, err = decode(node["body"])
		if err != nil {
			return nil, err
		}
		fd.Body = subnode.(fun.FuncBody)
		return &fd, nil
	default:
		return nil, fmt.Errorf("Not supported type: %v", t)
	}
}
