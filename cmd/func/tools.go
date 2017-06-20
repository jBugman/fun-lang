package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"io/ioutil"
	"os"
)

func parseGoAST(filename string, source []byte) (*ast.File, *token.FileSet) {
	fset := token.NewFileSet()
	tree, err := parser.ParseFile(fset, filename, source, 0)
	if err != nil {
		exit(err)
	}
	return tree, fset
}

func readInput(filename string, useStdin bool) []byte {
	var err error
	var result []byte
	var in io.ReadCloser
	if useStdin {
		in = ioutil.NopCloser(os.Stdin)
	} else {
		in, err = os.Open(filename)
		if err != nil {
			exit(err)
		}
	}
	defer in.Close()
	result, err = ioutil.ReadAll(in)
	if err != nil {
		exit(err)
	}
	return result
}

func writeOutput(text []byte, filename string, useStdout bool) {
	var err error
	var out io.WriteCloser
	if useStdout {
		out = os.Stdout
	} else {
		out, err = os.OpenFile(filename, os.O_WRONLY|os.O_CREATE, 0644)
		if err != nil {
			exit(err)
		}
	}
	_, err = out.Write(text)
	if err != nil {
		exit(err)
	}
	if !useStdout {
		out.Close()
	}
}

func exit(err interface{}) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}

// debugPrint prints Go AST
func debugPrint(tree *ast.File, fset *token.FileSet) string {
	var buf bytes.Buffer
	ast.Fprint(&buf, fset, tree, ast.NotNilFilter)
	return buf.String()
}
