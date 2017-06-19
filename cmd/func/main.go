package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/jBugman/fun-lang/translate"
)

func main() {
	// Flags
	var infile, outfile string
	var useStdout bool
	parseArgs(&infile, &outfile, &useStdout)

	// Read source
	var err error
	var source []byte
	source, err = ioutil.ReadFile(infile) // TODO optional Stdin
	if err != nil {
		exit(err)
	}

	// Parse Go AST
	tree, fset := parseGoAST(infile, source)
	// debugPrint(tree, fset)

	// Translate to Fun
	gofun := translate.NewFun(fset)
	module, err := gofun.Module(tree)
	if err != nil {
		exit(err)
	}
	result := fmt.Sprint(module)

	// Write output
	writeOutput(result, outfile, useStdout)
}

func debugPrint(tree *ast.File, fset *token.FileSet) string {
	var buf bytes.Buffer
	ast.Fprint(&buf, fset, tree, ast.NotNilFilter)
	return buf.String()
}

func parseGoAST(filename string, source []byte) (*ast.File, *token.FileSet) {
	fset := token.NewFileSet()
	tree, err := parser.ParseFile(fset, filename, source, 0)
	if err != nil {
		exit(err)
	}
	return tree, fset
}

func parseArgs(infile, outfile *string, useStdout *bool) {
	flag.StringVar(outfile, "o", "", "output file")
	flag.BoolVar(useStdout, "s", false, "use Stdout")
	flag.Parse()
	if flag.NArg() == 0 {
		exit("please provide source file")
	}
	*infile = flag.Arg(0)
	if *outfile == "" {
		*outfile = strings.TrimSuffix(*infile, filepath.Ext(*infile)) + ".fun"
	}
}

func writeOutput(text string, filename string, useStdout bool) {
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
	_, err = io.WriteString(out, text)
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
