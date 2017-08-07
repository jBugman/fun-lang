package parser_test

import (
	"strings"
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
		fun.L(pos(1, 1),
			fun.ID("foo", pos(1, 2)),
			fun.ID("bar", pos(1, 6)),
		),
	),

	Parsing("(foo (a))",
		fun.L(pos(1, 1),
			fun.ID("foo", pos(1, 2)),
			fun.L(
				pos(1, 6),
				fun.ID("a", pos(1, 7)),
			),
		),
	),

	Entry("func call",
		`(printf "%+v\n" v)`,
		fun.L(pos(1, 1),
			fun.KW("printf", pos(1, 2)),
			fun.SL("%+v\\n", pos(1, 9)),
			fun.ID("v", pos(1, 17)),
		),
	),

	Entry("selector + unit",
		`(fmt.Println ())`,
		fun.L(pos(1, 1),
			fun.ID("fmt.Println", pos(1, 2)),
			fun.LL(nil, pos(1, 14)),
		),
	),

	Entry("import",
		`(import "foo")`,
		fun.L(pos(1, 1),
			fun.KW("import", pos(1, 2)),
			fun.SL("foo", pos(1, 9)),
		),
	),

	XEntry("multiline s-exp",
		ml(
			`(+ foo bar`,
			`    :int)`,
		),
		fun.L(pos(1, 1),
			fun.OP("+", pos(1, 2)),
			fun.ID("foo", pos(1, 4)),
			fun.ID("bar", pos(1, 8)),
			fun.TP("int", pos(2, 5)),
		),
	),

	XEntry("multiline s-exp with a comment",
		ml(
			`(foo 123 456`,
			`; comment`,
			`  Bar)`,
		),
		fun.L(pos(1, 1),
			fun.ID("foo", pos(1, 2)),
			fun.IL("123", pos(1, 6)),
			fun.IL("456", pos(1, 10)),
			// TODO: comment
			fun.ID("Bar", pos(3, 3)),
		),
	),

	Entry("hello world",
		ml(
			`(package main`,
			``,
			`  (func main (print "hello world")))`,
		),
		fun.L(pos(1, 1),
			fun.KW("package", pos(1, 2)),
			fun.ID("main", pos(1, 10)),
			fun.L(pos(3, 3),
				fun.KW("func", pos(3, 4)),
				fun.ID("main", pos(3, 9)),
				fun.L(pos(3, 14),
					fun.KW("print", pos(3, 15)),
					fun.SL("hello world", pos(3, 21)),
				),
			),
		),
	),

	XEntry("var decl from slice literal",
		`(var t (:slice :string) ("g" "h" "c"))`,
		fun.L(pos(1, 1),
			fun.KW("var", pos(1, 2)),
			fun.ID("t", pos(1, 6)),
			fun.L(pos(1, 8),
				fun.TP("slice", pos(1, 9)),
				fun.TP("string", pos(1, 16)),
			),
			fun.L(pos(1, 25),
				fun.SL("g", pos(1, 26)),
				fun.SL("h", pos(1, 30)),
				fun.SL("c", pos(1, 34)),
			),
		),
	),

	XEntry("func type",
		`(:func () :int)`,
		fun.L(pos(1, 1),
			fun.TP("func", pos(1, 2)),
			fun.LL(nil, pos(1, 8)),
			fun.TP("int", pos(1, 11)),
		),
	),

	XEntry("type assert",
		`(assert :foo x)`,
		fun.L(pos(1, 1),
			fun.KW("assert", pos(1, 2)),
			fun.TP("foo", pos(1, 9)),
			fun.ID("x", pos(1, 13)),
		),
	),

	Entry("unit test assert",
		`(assert.Equal (s.T) text t)`,
		fun.L(pos(1, 1),
			fun.ID("assert.Equal", pos(1, 2)),
			fun.L(pos(1, 15),
				fun.ID("s.T", pos(1, 16)),
			),
			fun.ID("text", pos(1, 21)),
			fun.ID("t", pos(1, 26)),
		),
	),
)

func ml(lines ...string) string {
	return strings.Join(lines, "\n")
}

func pos(line, col int) code.Pos {
	return code.Pos{
		Line: line,
		Col:  col,
	}
}
