package parser

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/go/fun"
	"github.com/jBugman/fun-lang/go/fun/code"
)

func ident(x string, pos code.Pos) fun.Ident {
	return fun.NewIdent(x, pos)
}

func list(xs []fun.Expr, pos code.Pos) fun.List {
	return fun.NewList(xs, pos)
}

func pos(line, col int) code.Pos {
	return code.NewPos(line, col)
}

func TestParseExpression_0(t *testing.T) {
	source := newScanner("foo")
	x, s, err := parseExpression(source)
	assert.NoError(t, err)
	assert.Equal(t, ident("foo", pos(1, 1)), x)
	assert.Equal(t, pos(1, 4), s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestParseExpression_1(t *testing.T) {
	source := newScanner("foo bar")
	x, s, err := parseExpression(source)
	assert.NoError(t, err) // Should fail
	assert.Equal(t, ident("foo", pos(1, 1)), x)
	assert.Equal(t, pos(1, 4), s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestParseIdent_0(t *testing.T) {
	source := newScanner("foo bar")
	x, s, err := parseIdent(source)
	assert.NoError(t, err)
	assert.Equal(t, ident("foo", pos(1, 1)), x)
	assert.Equal(t, pos(1, 4), s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestParseIdent_1(t *testing.T) {
	source := newScanner("foo\n\nbar")
	x, s, err := parseIdent(source)
	assert.NoError(t, err)
	assert.Equal(t, ident("foo", pos(1, 1)), x)
	assert.Equal(t, pos(1, 4), s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestSkipSpace(t *testing.T) {
	source := newScanner("  \n\nbar")
	s := skipSpace(source)
	assert.Equal(t, pos(3, 1), s.pos)
	assert.Equal(t, 4, s.cursor)
}

func TestParseList_0(t *testing.T) {
	source := newScanner("(foo)")
	x, s, err := parseList(source)
	assert.NoError(t, err)
	assert.Equal(t, list(
		[]fun.Expr{
			ident("foo", pos(1, 2)),
		},
		pos(1, 1),
	), x)
	assert.Equal(t, pos(1, 6), s.pos)
	assert.Equal(t, 5, s.cursor)
}

func TestParseList_1(t *testing.T) {
	source := newScanner("(foo bar)")
	x, s, err := parseList(source)
	assert.NoError(t, err)
	assert.Equal(t, list(
		[]fun.Expr{
			ident("foo", pos(1, 2)),
			ident("bar", pos(1, 6)),
		},
		pos(1, 1),
	), x)
	assert.Equal(t, pos(1, 10), s.pos)
	assert.Equal(t, 9, s.cursor)
}

func TestParseList_3(t *testing.T) {
	source := newScanner("(foo ())")
	x, s, err := parseList(source)
	assert.NoError(t, err)
	assert.Equal(t, list(
		[]fun.Expr{
			ident("foo", pos(1, 2)),
			list(nil, pos(1, 6)),
		},
		pos(1, 1),
	), x)
	assert.Equal(t, pos(1, 9), s.pos)
	assert.Equal(t, 8, s.cursor)
}

func TestParseList_4(t *testing.T) {
	source := newScanner("()")
	x, s, err := parseList(source)
	assert.NoError(t, err)
	assert.Equal(t, list(nil, pos(1, 1)), x)
	assert.Equal(t, pos(1, 3), s.pos)
	assert.Equal(t, 2, s.cursor)
}

func TestParseList_5(t *testing.T) {
	source := newScanner("(foo (a))")
	x, s, err := parseList(source)
	assert.NoError(t, err)
	assert.Equal(t, list(
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
	), x)
	assert.Equal(t, pos(1, 10), s.pos)
	assert.Equal(t, 9, s.cursor)
}
