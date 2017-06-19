package main

import (
	"flag"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/jBugman/fun-lang/translate"
)

// Flags.
var (
	outfile   = flag.String("o", "", "output file")
	useStdout = flag.Bool("s", false, "use Stdout")
	useStdin  = flag.Bool("i", false, "use Stdin")
)

func main() {
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
		*outfile = strings.TrimSuffix(infile, filepath.Ext(infile)) + ".fun"
	}

	source := readInput(infile, *useStdin)

	// Parse Go AST
	tree, fset := parseGoAST(infile, source)

	// Translate to Fun
	module, err := translate.NewFun(fset).Module(tree)
	if err != nil {
		exit(err)
	}
	result := fmt.Sprint(module)

	// Write output
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
