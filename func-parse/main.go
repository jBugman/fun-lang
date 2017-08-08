package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/jBugman/fun-lang/func-parse/parser"
)

func main() {
	source, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintln(os.Stderr, errors.Wrap(err, "reading input"))
		os.Exit(1)
	}

	result, e := parser.Parse(source)
	if e != nil {
		const filename = "<standard input>"
		fmt.Fprintf(os.Stderr, "%s:%d:%d: %s\n", filename, e.Pos().Line, e.Pos().Col, e)
		os.Exit(1)
	}

	output, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		fmt.Fprintln(os.Stderr, errors.Wrap(err, "marshal json"))
		os.Exit(1)
	}
	os.Stdout.Write(output)
}
