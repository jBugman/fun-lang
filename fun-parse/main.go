package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/jBugman/fun-lang/fun-parse/parser"
)

func main() {
	filename := os.Args[1]
	source := readFile(filename)
	result, err := parser.Parse(source)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s:%d:%d: %s\n", filename, err.Pos().Line, err.Pos().Col, err)
		os.Exit(1)
	}

	fmt.Println(result)
}

func readFile(filename string) []byte {
	source, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Fprintln(os.Stderr, errors.Wrap(err, "read file"))
		os.Exit(1)
	}
	return source
}
