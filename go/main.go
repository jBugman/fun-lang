package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/pkg/errors"

	"github.com/jBugman/fun-lang/go/parser"
)

func main() {
	filename := os.Args[1]
	source, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Fprintln(os.Stderr, errors.Wrap(err, "read file"))
		os.Exit(1)
	}
	result, err := parser.Parse(source)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s:%s\n", filename, err)
		os.Exit(1)
	}

	fmt.Println(result)
}
