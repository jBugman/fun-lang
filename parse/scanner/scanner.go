// Package scanner provides Fun lexer.
package scanner

import (
	"bufio"
	"bytes"
	"io"
	"unicode"

	"github.com/jBugman/fun-lang/fun/tokens"
)

// Scanner represents Fun lexer.
type Scanner struct {
	r *bufio.Reader
	N int // read runes count
}

// NewScanner creates a Scanner.
func NewScanner(r io.Reader) *Scanner {
	return &Scanner{r: bufio.NewReader(r)}
}

/* Implementation */

// Scan returns the next token and literal value.
func (s *Scanner) Scan() (tokens.Token, string) {
	// Read the next rune.
	c := s.read()

	switch {
	case isWhitespace(c):
		// If we see whitespace we consume all contiguous whitespace.
		s.unread()
		return s.consumeWS()
		// If we see an ASCII letter we consume contiguous letters as an identifier or keyword.
		// TODO non-ASCII letters as string contents etc.
	case isASCIILetter(c):
		s.unread()
		return s.consumeWord()
	case isDigit(c):
		// If we see a digit we consume contiguous digits and such as a number.
		s.unread()
		tok, txt := s.consumeNumber()
		if tok != tokens.ILLEGAL {
			return tok, txt
		}
	}

	s.unread()
	tok, txt := s.consumeCompounds()
	if tok != tokens.ILLEGAL {
		return tok, txt
	}

	// Otherwise read the individual character.
	switch c {
	case eof:
		s.unread()
		return tokens.EOF, ""
	case '\n':
		return tokens.LF, string(c)
	case '"':
		return tokens.QUOTE, string(c)
	case '.':
		return tokens.PERIOD, string(c)
	case '=':
		return tokens.EQ, string(c)
	case '+':
		return tokens.PLUS, string(c)
	case '-':
		return tokens.DASH, string(c)
	case '/':
		return tokens.SLASH, string(c)
	case ':':
		return tokens.COLON, string(c)
	case '(':
		return tokens.OPENBR, string(c)
	case ')':
		return tokens.CLOSEBR, string(c)
	case ',':
		return tokens.COMMA, string(c)
	case '%':
		return tokens.PERCENT, string(c)
	}

	s.unread()
	return tokens.ILLEGAL, string(c)
}

// consumeWS consumes the current rune and all contiguous whitespace.
func (s *Scanner) consumeWS() (tokens.Token, string) {
	s.read()
LOOP:
	for {
		c := s.read()
		switch {
		case c == eof:
			s.unread()
			break LOOP
		case !isWhitespace(c):
			s.unread()
			break LOOP
		}
	}
	return tokens.WS, " " // Collapse all whitespace characters into a single space.
}

// consumeWord consumes the current rune and all contiguous letter runes.
func (s *Scanner) consumeWord() (tokens.Token, string) {
	var buf bytes.Buffer
	buf.WriteRune(s.read())

LOOP:
	for {
		c := s.read()
		switch {
		case c == eof:
			s.unread()
			break LOOP
		case !isASCIILetter(c) && !isDigit(c):
			// TODO separate cases for identifiers and just unicode strings
			s.unread()
			break LOOP
		default:
			buf.WriteRune(c)
		}
	}

	return checkKeywords(buf.String())
}

// consumeNumber consumes the current rune and all contiguous number-related runes.
func (s *Scanner) consumeNumber() (tokens.Token, string) {
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	var isFloat bool
LOOP:
	for {
		c := s.read()
		switch {
		case c == eof:
			s.unread()
			break LOOP
		case c == '.' && !isFloat:
			buf.WriteRune(c)
			isFloat = true
		case !isDigit(c) || isWhitespace(c) || c == '\n':
			s.unread()
			break LOOP
		default:
			buf.WriteRune(c)
		}
	}

	if isFloat {
		return tokens.FLOAT, buf.String()
	}
	return tokens.INTEGER, buf.String()
}

var compounds = []tokens.Token{}

// consumeCompounds consumes the current rune and all contiguous runes representing non-letter language constructs.
func (s *Scanner) consumeCompounds() (tokens.Token, string) {
	// Create two-rune word and compare with compound tokens.
	var buf bytes.Buffer
	c := s.read()
	buf.WriteRune(c)
	buf.WriteRune(s.read())
	word := buf.String()

	switch tokens.Token(word) {
	case tokens.ARROW:
		return tokens.ARROW, word
	case tokens.DOUBLECOLON:
		return tokens.DOUBLECOLON, word
	case tokens.UNIT:
		return tokens.UNIT, word
	default:
		s.unread()
		return tokens.ILLEGAL, string(c)
	}
}

func checkKeywords(word string) (tokens.Token, string) {
	switch tokens.Token(word) {
	case tokens.IO:
		return tokens.IO, word
	case tokens.IMPORT:
		return tokens.IMPORT, word
	case tokens.UNDEFINED:
		return tokens.UNDEFINED, word
	case tokens.WHERE:
		return tokens.WHERE, word
	case tokens.MODULE:
		return tokens.MODULE, word
	case tokens.AS:
		return tokens.AS, word
	default:
		// If not a keyword it must be an identifier.
		return tokens.IDENT, word
	}
}

// read reads and returns the next Unicode character.
func (s *Scanner) read() rune {
	s.N++
	c, _, err := s.r.ReadRune()
	if err != nil {
		return eof
	}
	return c
}

// unread places the previously read rune back on the reader.
func (s *Scanner) unread() {
	s.N--
	s.r.UnreadRune()
}

/* Helpers */

const eof = rune(0)

func isWhitespace(c rune) bool {
	return c == ' ' || c == '\t' || c == '\r' // || c == '\n'
}

func isASCIILetter(c rune) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

func isDigit(c rune) bool {
	return unicode.IsDigit(c)
}
