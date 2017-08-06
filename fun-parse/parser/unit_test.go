package parser

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/jBugman/fun-lang/fun-parse/fun"
	"github.com/jBugman/fun-lang/fun-parse/fun/code"
)

var _ = Describe("parseExpression", func() {

	It("parses 'foo'", func() {
		source := newScanner("foo")
		x, s, err := parseExpression(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.ID("foo", pos(1, 1)),
		))
		Expect(s.pos).To(Equal(
			pos(1, 4),
		))
		Expect(s.cursor).To(Equal(3))
	})

	It("parses 'foo bar'", func() {
		source := newScanner("foo bar")
		x, s, err := parseExpression(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.ID("foo", pos(1, 1)),
		))
		Expect(s.pos).To(Equal(
			pos(1, 4),
		))
		Expect(s.cursor).To(Equal(3))
	})
})

var _ = Describe("parseIdent", func() {

	It("parses 'foo bar'", func() {
		source := newScanner("foo bar")
		x, s, err := parseIdent(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.ID("foo", pos(1, 1)),
		))
		Expect(s.pos).To(Equal(
			pos(1, 4),
		))
		Expect(s.cursor).To(Equal(3))
	})

	It("parses 'foo\\n\\nbar'", func() {
		source := newScanner("foo\n\nbar")
		x, s, err := parseIdent(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.ID("foo", pos(1, 1)),
		))
		Expect(s.pos).To(Equal(
			pos(1, 4),
		))
		Expect(s.cursor).To(Equal(3))
	})

	It("fails on non-letter first char", func() {
		source := newScanner("99bottles")
		_, s, err := parseIdent(source)
		Expect(err).Should(HaveOccurred())
		Expect(err.Error()).Should(Equal("1:1: expected letter or '_', found '9'"))
		Expect(s.pos).To(Equal(
			pos(1, 1),
		))
		Expect(s.cursor).To(Equal(0))
	})
})

var _ = Describe("skipSpace", func() {

	It("skips space", func() {
		source := newScanner("  \n\nbar")
		s := skipSpace(source)

		Expect(s.pos).To(Equal(
			pos(3, 1),
		))
		Expect(s.cursor).To(Equal(4))
	})
})

var _ = Describe("parseList", func() {

	It("parses ()", func() {
		source := newScanner("()")
		x, s, err := parseList(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.LL(nil, pos(1, 1)),
		))
		Expect(s.pos).To(Equal(
			pos(1, 3)))
		Expect(s.cursor).To(Equal(2))
	})

	It("parses (foo)", func() {
		source := newScanner("(foo)")
		x, s, err := parseList(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.L(pos(1, 1),
				fun.ID("foo", pos(1, 2)),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 6),
		))
		Expect(s.cursor).To(Equal(5))
	})

	It("parses (foo bar)", func() {
		source := newScanner("(foo bar)")
		x, s, err := parseList(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.L(pos(1, 1),
				fun.ID("foo", pos(1, 2)),
				fun.ID("bar", pos(1, 6)),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 10),
		))
		Expect(s.cursor).To(Equal(9))
	})

	It("parses (foo ())", func() {
		source := newScanner("(foo ())")
		x, s, err := parseList(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.L(pos(1, 1),
				fun.ID("foo", pos(1, 2)),
				fun.LL(nil, pos(1, 6)),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 9),
		))
		Expect(s.cursor).To(Equal(8))
	})

	It("parses (foo (a))", func() {
		source := newScanner("(foo (a))")
		x, s, err := parseList(source)

		Expect(err).ShouldNot(HaveOccurred())
		Expect(x).To(Equal(
			fun.L(pos(1, 1),
				fun.ID("foo", pos(1, 2)),
				fun.L(pos(1, 6),
					fun.ID("a", pos(1, 7)),
				),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 10),
		))
		Expect(s.cursor).To(Equal(9))
	})
})

func pos(line, col int) code.Pos {
	return code.Pos{
		Line: line,
		Col:  col,
	}
}
