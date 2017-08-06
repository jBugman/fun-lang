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

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			ident(pos(1, 1),
				"foo"),
		))
		Expect(s.pos).To(Equal(
			pos(1, 4),
		))
		Expect(s.cursor).To(Equal(3))
	})

	It("parses 'foo bar'", func() {
		source := newScanner("foo bar")
		x, s, err := parseExpression(source)

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			ident(pos(1, 1),
				"foo"),
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

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			ident(pos(1, 1),
				"foo"),
		))
		Expect(s.pos).To(Equal(
			pos(1, 4),
		))
		Expect(s.cursor).To(Equal(3))
	})

	It("parses 'foo\\n\\nbar'", func() {
		source := newScanner("foo\n\nbar")
		x, s, err := parseIdent(source)

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			ident(pos(1, 1),
				"foo"),
		))
		Expect(s.pos).To(Equal(
			pos(1, 4),
		))
		Expect(s.cursor).To(Equal(3))
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

	It("()", func() {
		source := newScanner("()")
		x, s, err := parseList(source)

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			list(pos(1, 1)),
		))
		Expect(s.pos).To(Equal(
			pos(1, 3)))
		Expect(s.cursor).To(Equal(2))
	})

	It("(foo)", func() {
		source := newScanner("(foo)")
		x, s, err := parseList(source)

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			list(pos(1, 1),
				ident(pos(1, 2),
					"foo"),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 6),
		))
		Expect(s.cursor).To(Equal(5))
	})

	It("(foo bar)", func() {
		source := newScanner("(foo bar)")
		x, s, err := parseList(source)

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			list(pos(1, 1),
				ident(pos(1, 2),
					"foo"),
				ident(pos(1, 6),
					"bar"),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 10),
		))
		Expect(s.cursor).To(Equal(9))
	})

	It("(foo ())", func() {
		source := newScanner("(foo ())")
		x, s, err := parseList(source)

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			list(pos(1, 1),
				ident(pos(1, 2),
					"foo"),
				list(pos(1, 6)),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 9),
		))
		Expect(s.cursor).To(Equal(8))
	})

	It("(foo (a))", func() {
		source := newScanner("(foo (a))")
		x, s, err := parseList(source)

		Expect(err).Should(Succeed())
		Expect(x).To(Equal(
			list(pos(1, 1),
				ident(pos(1, 2),
					"foo"),
				list(pos(1, 6),
					ident(pos(1, 7),
						"a"),
				),
			),
		))
		Expect(s.pos).To(Equal(
			pos(1, 10),
		))
		Expect(s.cursor).To(Equal(9))
	})
})

func ident(pos code.Pos, x string) fun.Ident {
	return fun.NewIdent(x, pos)
}

func list(pos code.Pos, xs ...fun.Expr) fun.List {
	return fun.NewList(xs, pos)
}

func pos(line, col int) code.Pos {
	return code.NewPos(line, col)
}
