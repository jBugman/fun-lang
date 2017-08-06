package testing

import (
	"github.com/onsi/ginkgo/extensions/table"
	g "github.com/onsi/gomega"

	"github.com/jBugman/fun-lang/fun-parse/fun"
	"github.com/jBugman/fun-lang/fun-parse/fun/code"
	"github.com/jBugman/fun-lang/fun-parse/parser"
)

// Parse is a wrapper for parser.Parse.
func Parse(source string) (fun.Expr, error) {
	return parser.Parse([]byte(source))
}

// PV is a validator for parser cases.
func PV(source string, result fun.Expr) {
	res, err := Parse(source)
	g.Expect(err).Should(g.Succeed())
	g.Expect(res).To(g.Equal(result))
}

// Parsing creates parser test table entry.
func Parsing(source string, actual fun.Expr) table.TableEntry {
	return table.Entry(source, source, actual)
}

// Type shortcuts

// L creates fun.List.
func L(pos code.Pos, xs ...fun.Expr) fun.List {
	return fun.NewList(xs, pos)
}

// ID creates fun.Ident.
func ID(x string, pos code.Pos) fun.Ident {
	return fun.NewIdent(x, pos)
}

// Pos creates code.Pos.
func Pos(line, col int) code.Pos {
	return code.NewPos(line, col)
}
