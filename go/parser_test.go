package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseExpression_0(t *testing.T) {
	src := newScanner("foo")
	x, s, err := parseExpression(src)
	assert.NoError(t, err)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 4}, s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestParseExpression_1(t *testing.T) {
	src := newScanner("foo bar")
	x, s, err := parseExpression(src)
	assert.NoError(t, err) // Should fail
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 4}, s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestParseIdent_0(t *testing.T) {
	src := newScanner("foo bar")
	x, s, err := parseIdent(src)
	assert.NoError(t, err)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 4}, s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestParseIdent_1(t *testing.T) {
	src := newScanner("foo\n\nbar")
	x, s, err := parseIdent(src)
	assert.NoError(t, err)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 4}, s.pos)
	assert.Equal(t, 3, s.cursor)
}

func TestSkipSpace(t *testing.T) {
	src := newScanner("  \n\nbar")
	s := skipSpace(src)
	assert.Equal(t, Pos{3, 1}, s.pos)
	assert.Equal(t, 4, s.cursor)
}

func TestParseList_0(t *testing.T) {
	src := newScanner("(foo)")
	x, s, err := parseList(src)
	assert.NoError(t, err)
	assert.Equal(t, List{
		xs: []Expr{
			Ident{x: "foo", pos: Pos{1, 2}},
		},
		pos: Pos{1, 1},
	}, x)
	assert.Equal(t, Pos{1, 6}, s.pos)
	assert.Equal(t, 5, s.cursor)
}

func TestParseList_1(t *testing.T) {
	src := newScanner("(foo bar)")
	x, s, err := parseList(src)
	assert.NoError(t, err)
	assert.Equal(t, List{
		xs: []Expr{
			Ident{x: "foo", pos: Pos{1, 2}},
			Ident{x: "bar", pos: Pos{1, 6}},
		},
		pos: Pos{1, 1},
	}, x)
	assert.Equal(t, Pos{1, 10}, s.pos)
	assert.Equal(t, 9, s.cursor)
}

func TestParseList_3(t *testing.T) {
	src := newScanner("(foo ())")
	x, s, err := parseList(src)
	assert.NoError(t, err)
	assert.Equal(t, List{
		xs: []Expr{
			Ident{x: "foo", pos: Pos{1, 2}},
			List{pos: Pos{1, 6}},
		},
		pos: Pos{1, 1},
	}, x)
	assert.Equal(t, Pos{1, 9}, s.pos)
	assert.Equal(t, 8, s.cursor)
}

func TestParseList_4(t *testing.T) {
	src := newScanner("()")
	x, s, err := parseList(src)
	assert.NoError(t, err)
	assert.Equal(t, List{pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 3}, s.pos)
	assert.Equal(t, 2, s.cursor)
}

func TestParseList_5(t *testing.T) {
	src := newScanner("(foo (a))")
	x, s, err := parseList(src)
	assert.NoError(t, err)
	assert.Equal(t, List{
		xs: []Expr{
			Ident{x: "foo", pos: Pos{1, 2}},
			List{
				xs: []Expr{
					Ident{x: "a", pos: Pos{1, 7}},
				},
				pos: Pos{1, 6},
			},
		},
		pos: Pos{1, 1},
	}, x)
	assert.Equal(t, Pos{1, 10}, s.pos)
	assert.Equal(t, 9, s.cursor)
}

func TestParser_0(t *testing.T) {
	src := []byte("foo")
	res, err := parseSource(src)
	assert.NoError(t, err)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, res)
}

func TestParser_1(t *testing.T) {
	src := []byte("foo bar")
	_, err := parseSource(src)
	assert.EqualError(t, err, "1:4: expected 'EOF', found ' '")
}

func TestParser_2(t *testing.T) {
	src := []byte("foo\n\nbar")
	_, err := parseSource(src)
	assert.EqualError(t, err, "1:4: expected 'EOF', found '\\n'")
}

func TestParser_3(t *testing.T) {
	src := []byte("  \n\nbar")
	res, err := parseSource(src)
	assert.NoError(t, err)
	assert.Equal(t, Ident{x: "bar", pos: Pos{3, 1}}, res)
}

func TestParser_4(t *testing.T) {
	src := []byte("(foo)")
	res, err := parseSource(src)
	assert.NoError(t, err)
	assert.Equal(t, List{
		xs: []Expr{
			Ident{x: "foo", pos: Pos{1, 2}},
		},
		pos: Pos{1, 1},
	}, res)
}

func TestParser_5(t *testing.T) {
	src := []byte("(foo bar)")
	res, err := parseSource(src)
	assert.NoError(t, err)
	assert.Equal(t, List{
		xs: []Expr{
			Ident{x: "foo", pos: Pos{1, 2}},
			Ident{x: "bar", pos: Pos{1, 6}},
		},
		pos: Pos{1, 1},
	}, res)
}

func TestParser_6(t *testing.T) {
	src := []byte("()")
	res, err := parseSource(src)
	assert.NoError(t, err)
	assert.Equal(t, List{pos: Pos{1, 1}}, res)
}

func TestParser_7(t *testing.T) {
	src := []byte("(foo (a))")
	res, err := parseSource(src)
	assert.NoError(t, err)
	assert.Equal(t, List{
		xs: []Expr{
			Ident{x: "foo", pos: Pos{1, 2}},
			List{
				xs: []Expr{
					Ident{x: "a", pos: Pos{1, 7}},
				},
				pos: Pos{1, 6},
			},
		},
		pos: Pos{1, 1},
	}, res)
}
