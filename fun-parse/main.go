package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/jBugman/fun-lang/fun-parse/parser"
)

func main() {
	filename := os.Args[1]
	source := readFile(filename)

	result, e := parser.Parse(source)
	if e != nil {
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

func readFile(filename string) []byte {
	source, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Fprintln(os.Stderr, errors.Wrap(err, "read file"))
		os.Exit(1)
	}
	return source
}
