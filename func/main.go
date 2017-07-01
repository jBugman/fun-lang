package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/jBugman/fun-lang/parse"
	"github.com/jBugman/fun-lang/print"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: func [filename.fun]")
		return
	}
	infile := os.Args[1]
	outfile := strings.TrimSuffix(infile, filepath.Ext(infile)) + ".go"
	var err error

	// Read from source
	source, err := ioutil.ReadFile(infile)
	if err != nil {
		exit(err)
	}

	// Parse Fun AST
	ast, err := parse.Package(source)
	if err != nil {
		exit(err)
	}

	// Translate to Go
	result, err := print.Package(*ast)
	if err != nil {
		exit(err)
	}

	// Write result to file
	err = ioutil.WriteFile(outfile, result, 0644)
	if err != nil {
		exit(err)
	}
}

func exit(err interface{}) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}
