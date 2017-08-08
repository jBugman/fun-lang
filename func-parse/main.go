package main

import (
	"encoding/json"
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/jBugman/fun-lang/func-parse/fun/code"
	"github.com/jBugman/fun-lang/func-parse/parser"
)

func main() {
	source, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		abortWith(errors.Wrap(err, "reading stdin"))
	}

	result, e := parser.Parse(source)
	if e != nil {
		abortWithJ(jsonError{
			Error: e.Error(),
			Pos:   e.Pos(),
		})
	}

	output, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		abortWith(errors.Wrap(err, "marshal json"))
	}
	os.Stdout.Write(output)
}

type jsonError struct {
	Error string   `json:"error"`
	Pos   code.Pos `json:"pos,omitempty"`
}

func abortWith(err error) {
	abortWithJ(jsonError{Error: err.Error()})
}

func abortWithJ(err jsonError) {
	output, _ := json.MarshalIndent(err, "", "  ")
	os.Stderr.Write(output)
	os.Stderr.WriteString("\n")
	os.Exit(1)
}
