package parser_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/go/fun"
	"github.com/jBugman/fun-lang/go/fun/code"
	"github.com/jBugman/fun-lang/go/parser"
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

func TestParser_0(t *testing.T) {
	source := []byte("foo")
	res, err := parser.Parse(source)
	assert.NoError(t, err)
	assert.Equal(t, ident("foo", pos(1, 1)), res)
}

func TestParser_1(t *testing.T) {
	source := []byte("foo bar")
	_, err := parser.Parse(source)
	assert.EqualError(t, err, "1:4: expected 'EOF', found ' '")
}

func TestParser_2(t *testing.T) {
	source := []byte("foo\n\nbar")
	_, err := parser.Parse(source)
	assert.EqualError(t, err, "1:4: expected 'EOF', found '\\n'")
}

func TestParser_3(t *testing.T) {
	source := []byte("  \n\nbar")
	res, err := parser.Parse(source)
	assert.NoError(t, err)
	assert.Equal(t, ident("bar", pos(3, 1)), res)
}

func TestParser_4(t *testing.T) {
	source := []byte("(foo)")
	res, err := parser.Parse(source)
	assert.NoError(t, err)
	assert.Equal(t, list(
		[]fun.Expr{
			ident("foo", pos(1, 2)),
		},
		pos(1, 1),
	), res)
}

func TestParser_5(t *testing.T) {
	source := []byte("(foo bar)")
	res, err := parser.Parse(source)
	assert.NoError(t, err)
	assert.Equal(t, list(
		[]fun.Expr{
			ident("foo", pos(1, 2)),
			ident("bar", pos(1, 6)),
		},
		pos(1, 1),
	), res)
}

func TestParser_6(t *testing.T) {
	source := []byte("()")
	res, err := parser.Parse(source)
	assert.NoError(t, err)
	assert.Equal(t, list(nil, pos(1, 1)), res)
}

func TestParser_7(t *testing.T) {
	source := []byte("(foo (a))")
	res, err := parser.Parse(source)
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
	), res)
}
