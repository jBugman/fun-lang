package scanner_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun/tokens"
	"github.com/jBugman/fun-lang/parse/scanner"
)

// stateless helper
func scan(text string) (tokens.Token, string, *scanner.Scanner) {
	s := scanner.NewScanner(strings.NewReader(text))
	tok, txt := s.Scan()
	return tok, txt, s
}

func TestScanner_Scan_empty(t *testing.T) {
	tok, txt, _ := scan("")
	assert.Equal(t, tokens.EOF, tok)
	assert.Empty(t, txt)
}

func TestScanner_Scan_longWhitespace(t *testing.T) {
	tok, txt, _ := scan("     	  	 ")
	assert.Equal(t, tokens.WS, tok)
	assert.Equal(t, txt, " ")
}

func TestScanner_Scan_integer(t *testing.T) {
	tok, txt, _ := scan("42")
	assert.Equal(t, tokens.INTEGER, tok)
	assert.Equal(t, "42", txt)
}

func TestScanner_Scan_twoIntegers(t *testing.T) {
	tok, txt, s := scan("123 45")
	assert.Equal(t, tokens.INTEGER, tok)
	assert.Equal(t, "123", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	assert.Equal(t, " ", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.INTEGER, tok)
	assert.Equal(t, "45", txt)
}

func TestScanner_Scan_Float(t *testing.T) {
	tok, txt, _ := scan("36.6")
	assert.Equal(t, tokens.FLOAT, tok)
	assert.Equal(t, "36.6", txt)
}

// broken for now
func TestScanner_Scan_fakeNumber(t *testing.T) {
	tok, txt, _ := scan("99bottles")
	assert.Equal(t, tokens.ILLEGAL, tok)
	assert.Equal(t, "9", txt)
}

func TestScanner_Scan_words(t *testing.T) {
	tok, txt, s := scan("some words")
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "some", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	assert.Equal(t, " ", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "words", txt)
}

func TestScanner_Scan_moduleDeclaration(t *testing.T) {
	tok, txt, s := scan("module Main where\n")
	assert.Equal(t, tokens.MODULE, tok)
	assert.Equal(t, "module", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	assert.Equal(t, " ", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "Main", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	assert.Equal(t, " ", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.WHERE, tok)
	assert.Equal(t, "where", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.LF, tok)
	assert.Equal(t, "\n", txt)
}

func TestScanner_Scan_Arrow(t *testing.T) {
	tok, txt, _ := scan("->")
	assert.Equal(t, tokens.ARROW, tok)
	assert.Equal(t, "->", txt)
}

func TestScanner_Scan_braces(t *testing.T) {
	tok, txt, s := scan("(())")
	assert.Equal(t, tokens.OPENBR, tok)
	assert.Equal(t, "(", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.UNIT, tok)
	assert.Equal(t, "()", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.CLOSEBR, tok)
	assert.Equal(t, ")", txt)
}

func TestScanner_Scan_funcDecl(t *testing.T) {
	tok, txt, s := scan("f :: int -> IO ()\nf x = undefined")
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "f", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.DOUBLECOLON, tok)
	assert.Equal(t, "::", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "int", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.ARROW, tok)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IO, tok)
	assert.Equal(t, "IO", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.UNIT, tok)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.LF, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "f", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "x", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.EQ, tok)
	assert.Equal(t, "=", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.UNDEFINED, tok)
	assert.Equal(t, "undefined", txt)
}

func TestScanner_Scan_import(t *testing.T) {
	tok, txt, s := scan(`import "log15" as "log"`)
	assert.Equal(t, tokens.IMPORT, tok)
	assert.Equal(t, "import", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.QUOTE, tok)
	assert.Equal(t, "\"", txt)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "log15", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.QUOTE, tok)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.AS, tok)
	assert.Equal(t, "as", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.WS, tok)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.QUOTE, tok)
	tok, txt = s.Scan()
	assert.Equal(t, tokens.IDENT, tok)
	assert.Equal(t, "log", txt)
	tok, _ = s.Scan()
	assert.Equal(t, tokens.QUOTE, tok)
}

func TestScanner_Scan_42(t *testing.T) {
	tok, txt, _ := scan("42\n")
	assert.Equal(t, tokens.INTEGER, tok)
	assert.Equal(t, "42", txt)
}
