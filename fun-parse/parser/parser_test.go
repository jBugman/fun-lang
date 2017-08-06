package parser_test

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"

	"github.com/jBugman/fun-lang/fun-parse/fun"
	"github.com/jBugman/fun-lang/fun-parse/fun/code"
	"github.com/jBugman/fun-lang/fun-parse/parser"
)

func TestParser(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Parser Suite")
}

var _ = Describe("parser", func() {

	It("fails on 'foo bar'", func() {
		_, err := parse("foo bar")
		Expect(err).Should(MatchError("1:4: expected 'EOF', found ' '"))
	})

	It("fails on 'foo\\n\\nbar'", func() {
		_, err := parse("foo\n\nbar")
		Expect(err).Should(MatchError("1:4: expected 'EOF', found '\\n'"))
	})
})

var _ = DescribeTable("parse table", parseValidator,

	parsing("foo",
		ident("foo", pos(1, 1))),

	parsing("  \n\nbar",
		ident("bar", pos(3, 1))),

	parsing("()",
		list(nil, pos(1, 1))),

	parsing("(foo)",
		list(
			[]fun.Expr{
				ident("foo", pos(1, 2)),
			},
			pos(1, 1),
		)),

	parsing("(foo bar)",
		list(
			[]fun.Expr{
				ident("foo", pos(1, 2)),
				ident("bar", pos(1, 6)),
			},
			pos(1, 1),
		)),

	parsing("(foo (a))",
		list(
			[]fun.Expr{
				ident("foo", pos(1, 2)),
				list(
					[]fun.Expr{
						ident("a", pos(1, 7)),
					},
					pos(1, 6),
				),
			},
			pos(1, 1),
		)),
)

func parseValidator(source string, result fun.Expr) {
	res, err := parse(source)
	Expect(err).Should(Succeed())
	Expect(res).To(Equal(result))
}

// Utils

func parse(source string) (fun.Expr, error) {
	return parser.Parse([]byte(source))
}

func parsing(source string, actual fun.Expr) TableEntry {
	return Entry(source, source, actual)
}

// Type shortcuts

func ident(x string, pos code.Pos) fun.Ident {
	return fun.NewIdent(x, pos)
}

func list(xs []fun.Expr, pos code.Pos) fun.List {
	return fun.NewList(xs, pos)
}

func pos(line, col int) code.Pos {
	return code.NewPos(line, col)
}
