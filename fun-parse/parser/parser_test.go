package parser_test

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"

	"github.com/jBugman/fun-lang/fun-parse/fun"
	"github.com/jBugman/fun-lang/fun-parse/fun/code"
	. "github.com/jBugman/fun-lang/fun-parse/testing"
)

func TestParser(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Parser Suite")
}

// FIXME: pendings

var _ = Describe("parser", func() {

	It("fails on american double", func() {
		_, err := Parse(".223")
		Expect(err).Should(HaveOccurred())
	})

	It("fails on non-singleton char lit", func() {
		_, err := Parse("'foo'")
		Expect(err).Should(HaveOccurred())
	})

	It("fails on empty string", func() {
		_, err := Parse("")
		Expect(err).Should(HaveOccurred())
	})

	It("fails on garbage input", func() {
		_, err := Parse("_BANG!!")
		Expect(err).Should(HaveOccurred())
		Expect(err.Error()).To(Equal(
			"1:6: expected EOF, found '!'"))
	})

	It("fails on 'foo bar'", func() {
		_, err := Parse("foo bar")
		Expect(err).Should(MatchError("1:4: expected EOF, found ' '"))
	})

	It("fails on 'foo\\n\\nbar'", func() {
		_, err := Parse("foo\n\nbar")
		Expect(err).Should(MatchError("1:4: expected EOF, found '\\n'"))
	})
})

var _ = DescribeTable("parsing of", PV,

	Parsing("()",
		fun.LL(nil, pos(1, 1)),
	),

	Parsing("(())",
		fun.L(pos(1, 1),
			fun.LL(nil, pos(1, 2)),
		),
	),

	Entry("string lit",
		"\"test\"",
		fun.SL("test", pos(1, 1)),
	),

	Entry("raw string lit",
		"`l1\n\"2\"`",
		fun.String{X: "l1\n\"2\"", Pos: pos(1, 1), Raw: true},
	),

	Entry("char lit",
		"'z'",
		fun.CL("z", pos(1, 1)),
	),

	Entry("tick char",
		"'\\''",
		fun.CL("\\'", pos(1, 1)),
	),

	Entry("newline char",
		"'\\n'",
		fun.CL("\\n", pos(1, 1)),
	),

	Parsing("true",
		fun.BL(true, pos(1, 1)),
	),

	Parsing("false",
		fun.BL(false, pos(1, 1)),
	),

	XParsing("42",
		fun.IL("42", pos(1, 1)),
	),

	XParsing("0",
		fun.IL("0", pos(1, 1)),
	),

	XParsing("42.0",
		fun.DL("42.0", pos(1, 1)),
	),

	XParsing("1e3",
		fun.DL("1e3", pos(1, 1)),
	),

	XParsing("0644",
		fun.OL("0644", pos(1, 1)),
	),

	XParsing("0x2A",
		fun.HL("0x2A", pos(1, 1)),
	),

	Parsing("foo",
		fun.ID("foo", pos(1, 1)),
	),

	Parsing("_",
		fun.ID("_", pos(1, 1)),
	),

	Parsing("  \n\nbar",
		fun.ID("bar", pos(3, 1)),
	),

	Parsing("fmt.Println",
		fun.ID("fmt.Println", pos(1, 1)),
	),

	XParsing(":int",
		fun.TP("int", pos(1, 1)),
	),

	Parsing("+",
		fun.OP("+", pos(1, 1)),
	),

	Parsing("++",
		fun.OP("++", pos(1, 1)),
	),

	Parsing("&",
		fun.OP("&", pos(1, 1)),
	),

	Parsing("&&",
		fun.OP("&&", pos(1, 1)),
	),

	Parsing("|",
		fun.OP("|", pos(1, 1)),
	),

	Parsing("||",
		fun.OP("||", pos(1, 1)),
	),

	Parsing("!",
		fun.OP("!", pos(1, 1)),
	),

	Parsing("!=",
		fun.OP("!=", pos(1, 1)),
	),

	Parsing("(+ foo)",
		fun.L(pos(1, 1),
			fun.OP("+", pos(1, 2)),
			fun.ID("foo", pos(1, 4)),
		),
	),

	Parsing("(foo \"bar\")",
		fun.L(pos(1, 1),
			fun.ID("foo", pos(1, 2)),
			fun.SL("bar", pos(1, 6)),
		),
	),

	Parsing("(foo bar baz)",
		fun.L(pos(1, 1),
			fun.ID("foo", pos(1, 2)),
			fun.ID("bar", pos(1, 6)),
			fun.ID("baz", pos(1, 10)),
		),
	),

	XParsing("(== 0xff 255)",
		fun.L(pos(1, 1),
			fun.OP("==", pos(1, 2)),
			fun.HL("0xff", pos(1, 5)),
			fun.IL("255", pos(1, 10)),
		),
	),

	Entry("keyword",
		"for",
		fun.KW("for", pos(1, 1)),
	),

	Entry("not a keyword",
		"forall",
		fun.ID("forall", pos(1, 1)),
	),

	Parsing("(foo)",
		fun.L(
			pos(1, 1),
			fun.ID("foo", pos(1, 2)),
		),
	),

	Parsing("(foo bar)",
		fun.L(
			pos(1, 1),
			fun.ID("foo", pos(1, 2)),
			fun.ID("bar", pos(1, 6)),
		),
	),

	Parsing("(foo (a))",
		fun.L(
			pos(1, 1),
			fun.ID("foo", pos(1, 2)),
			fun.L(
				pos(1, 6),
				fun.ID("a", pos(1, 7)),
			),
		),
	),
)

func pos(line, col int) code.Pos {
	return code.Pos{
		Line: line,
		Col:  col,
	}
}
