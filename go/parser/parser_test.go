package parser_test

/*
import (
	"testing"

	"github.com/jBugman/fun-lang/go/parser"
	"github.com/stretchr/testify/assert"
)

func TestParser_0(t *testing.T) {
	src := []byte("foo")
	res, err := parser.Parse(src)
	assert.NoError(t, err)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, res)
}

func TestParser_1(t *testing.T) {
	src := []byte("foo bar")
	_, err := parser.Parse(src)
	assert.EqualError(t, err, "1:4: expected 'EOF', found ' '")
}

func TestParser_2(t *testing.T) {
	src := []byte("foo\n\nbar")
	_, err := parser.Parse(src)
	assert.EqualError(t, err, "1:4: expected 'EOF', found '\\n'")
}

func TestParser_3(t *testing.T) {
	src := []byte("  \n\nbar")
	res, err := parser.Parse(src)
	assert.NoError(t, err)
	assert.Equal(t, Ident{x: "bar", pos: Pos{3, 1}}, res)
}

func TestParser_4(t *testing.T) {
	src := []byte("(foo)")
	res, err := parser.Parse(src)
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
	res, err := parser.Parse(src)
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
	res, err := parser.Parse(src)
	assert.NoError(t, err)
	assert.Equal(t, List{pos: Pos{1, 1}}, res)
}

func TestParser_7(t *testing.T) {
	src := []byte("(foo (a))")
	res, err := parser.Parse(src)
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
*/
