package scanner_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/jBugman/fun-lang/parse/scanner"
	"github.com/jBugman/fun-lang/parse/tokens"
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
	assert.NotEmpty(t, txt)
}

func TestScanner_Scan_integer(t *testing.T) {
	tok, txt, _ := scan("42")
	assert.Equal(t, tokens.INTEGER, tok)
	assert.Equal(t, "42", txt)
}

// broken for now
// func TestScanner_Scan_fakeNumber(t *testing.T) {
// 	tok, txt, _ := scan("99bottles")
// 	assert.Equal(t, tokens.IDENT, tok)
// 	assert.Equal(t, "99bottles", txt)
// }

func TestScanner_Scan_words(t *testing.T) {
	tok, txt, s := scan("some words")
	require.Equal(t, tokens.IDENT, tok)
	require.Equal(t, "some", txt)
	tok, txt = s.Scan()
	require.Equal(t, tokens.WS, tok)
	require.Equal(t, " ", txt)
	tok, txt = s.Scan()
	require.Equal(t, tokens.IDENT, tok)
	require.Equal(t, "words", txt)
}
