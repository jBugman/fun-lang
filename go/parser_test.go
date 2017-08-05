package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseIdent_0(t *testing.T) {
	src := newScanner([]rune("foo"))
	x, s, err := parseIdent(src)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 4}, s.pos)
	assert.Equal(t, 3, s.cursor)
	assert.NoError(t, err)
}

func TestParseIdent_1(t *testing.T) {
	src := newScanner([]rune("foo bar"))
	x, s, err := parseIdent(src)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 4}, s.pos)
	assert.Equal(t, 3, s.cursor)
	assert.NoError(t, err)
}

func TestParseIdent_2(t *testing.T) {
	src := newScanner([]rune("foo\n\nbar"))
	x, s, err := parseIdent(src)
	assert.Equal(t, Ident{x: "foo", pos: Pos{1, 1}}, x)
	assert.Equal(t, Pos{1, 4}, s.pos)
	assert.Equal(t, 3, s.cursor)
	assert.NoError(t, err)
}

func TestSkipSpace(t *testing.T) {
	src := newScanner([]rune("  \n\nbar"))
	s := skipSpace(src)
	assert.Equal(t, Pos{3, 1}, s.pos)
	assert.Equal(t, 4, s.cursor)
}
