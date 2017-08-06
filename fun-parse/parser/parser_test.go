package parser_test

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"

	. "github.com/jBugman/fun-lang/fun-parse/testing"
)

func TestParser(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Parser Suite")
}

var _ = Describe("parser", func() {

	It("fails on 'foo bar'", func() {
		_, err := Parse("foo bar")
		Expect(err).Should(MatchError("1:4: expected 'EOF', found ' '"))
	})

	It("fails on 'foo\\n\\nbar'", func() {
		_, err := Parse("foo\n\nbar")
		Expect(err).Should(MatchError("1:4: expected 'EOF', found '\\n'"))
	})
})

var _ = DescribeTable("parse cases", PV,

	Parsing("foo",
		ID("foo", Pos(1, 1))),

	Parsing("  \n\nbar",
		ID("bar", Pos(3, 1))),

	Parsing("()",
		L(
			Pos(1, 1),
			// nil
		)),

	Parsing("(foo)",
		L(
			Pos(1, 1),
			ID("foo", Pos(1, 2)),
		)),

	Parsing("(foo bar)",
		L(
			Pos(1, 1),
			ID("foo", Pos(1, 2)),
			ID("bar", Pos(1, 6)),
		)),

	Parsing("(foo (a))",
		L(
			Pos(1, 1),
			ID("foo", Pos(1, 2)),
			L(
				Pos(1, 6),
				ID("a", Pos(1, 7)),
			),
		)),
)
