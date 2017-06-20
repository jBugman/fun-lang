package main

import (
	"flag"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/jBugman/fun-lang/parse"
	"github.com/jBugman/fun-lang/print"
	"github.com/jBugman/fun-lang/translate"
)

// Flags.
var (
	outfile   = flag.String("o", "", "output file")
	useStdout = flag.Bool("s", false, "use Stdout")
	useStdin  = flag.Bool("i", false, "use Stdin")
	fromGo    = flag.Bool("go", false, "translate from Go")
)

func main() {
	// Setting up flags and variables.
	flag.Parse()
	var infile string
	if *useStdin {
		infile = "out.fun"
	} else {
		if flag.NArg() == 0 {
			exit("please provide source file")
		}
		infile = flag.Arg(0)
	}
	if *outfile == "" { // flag not provided
		var ext = ".go"
		if *fromGo {
			ext = ".fun"
		}
		*outfile = strings.TrimSuffix(infile, filepath.Ext(infile)) + ext
	}

	// Reading from source.
	source := readInput(infile, *useStdin)

	var result []byte
	if *fromGo {
		// Parse Go AST.
		tree, fset := parseGoAST(infile, source)

		// Translate to Fun.
		module, err := translate.NewFun(fset).Module(tree)
		if err != nil {
			exit(err)
		}
		result = []byte(fmt.Sprint(module))
	} else {
		// Parse Fun AST
		tree, err := parse.Bytes(source)
		if err != nil {
			exit(err)
		}
		result, err = print.Module(*tree)
		if err != nil {
			exit(err)
		}
	}

	// Write result to file or Stdout
	writeOutput(result, *outfile, *useStdout)
}

func parseArgs(infile, outfile *string, useStdout *bool) {
	flag.StringVar(outfile, "o", "", "output file")
	flag.BoolVar(useStdout, "s", false, "use Stdout")
	flag.Parse()
	if flag.NArg() == 0 {
		exit("please provide source file")
	}
}
