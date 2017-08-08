package testing

import (
	"github.com/onsi/ginkgo/extensions/table"
	g "github.com/onsi/gomega"

	"github.com/jBugman/fun-lang/func-parse/fun"
	"github.com/jBugman/fun-lang/func-parse/parser"
)

// Parse is a wrapper for parser.Parse.
func Parse(source string) (fun.Expr, parser.ParseError) {
	return parser.Parse([]byte(source))
}

// PV is a validator for parser cases.
func PV(source string, result fun.Expr) {
	res, err := Parse(source)
	g.Expect(err).ShouldNot(g.HaveOccurred())
	g.Expect(res).To(g.Equal(result))
}

// Parsing creates parser test table entry.
func Parsing(source string, actual fun.Expr) table.TableEntry {
	return table.Entry(source, source, actual)
}

// XParsing creates pending parser test table entry.
func XParsing(source string, actual fun.Expr) table.TableEntry {
	return table.XEntry(source, source, actual)
}
