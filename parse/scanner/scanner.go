package scanner

import (
	"bufio"
	"bytes"
	"io"
	"unicode"

	"github.com/jBugman/fun-lang/fun/tokens"
)

// Scanner eepresents Fun lexer.
type Scanner struct {
	r *bufio.Reader
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
		// If we see an ASCII letter wr consume contiguous letteers as an identifier or keyword.
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

	// Otherwise read the individual character.
	switch c {
	case eof:
		return tokens.EOF, ""
	case '\n':
		return tokens.LF, string(c)
	case '.':
		return tokens.PERIOD, string(c)
	case '=':
		return tokens.EQ, string(c)
	case ':':
		return tokens.COLON, string(c)
	case '(':
		return tokens.OPENBR, string(c)
	case ')':
		return tokens.CLOSEBR, string(c)
	case ',':
		return tokens.COMMA, string(c)
	}

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
			break LOOP
		case !isASCIILetter(c) && !isDigit(c):
			// TOOD separate cases for identifiers and just unicode strings
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

	var n int
	var isFloat bool
LOOP:
	for {
		c := s.read()
		switch {
		case c == eof:
			break LOOP
		case c == '.' && !isFloat:
			buf.WriteRune(c)
			isFloat = true
		case isWhitespace(c):
			s.unread()
			break LOOP
		case !isDigit(c):
			// Not a number, rollback
			for n > 0 {
				s.unread()
				n--
			}
			isFloat = false
			return tokens.ILLEGAL, string(c)
		default:
			buf.WriteRune(c)
			n++
		}
	}

	if isFloat {
		return tokens.FLOAT, buf.String()
	}
	return tokens.INTEGER, buf.String()
}

func checkKeywords(word string) (tokens.Token, string) {
	switch word {
	// TODO consolidate literals into tokens package
	case string(tokens.IO):
		return tokens.IO, word
	case string(tokens.IMPORT):
		return tokens.IMPORT, word
	case string(tokens.UNDEFINED):
		return tokens.UNDEFINED, word
	case string(tokens.WHERE):
		return tokens.WHERE, word
	case string(tokens.MODULE):
		return tokens.MODULE, word
	default:
		// If not a keyword it must be an identifier
		return tokens.IDENT, word
	}
}

// read reads and returns the next Unicode character.
func (s *Scanner) read() rune {
	c, _, err := s.r.ReadRune()
	if err != nil {
		return eof
	}
	return c
}

// unread places the previously read rune back on the reader.
func (s *Scanner) unread() {
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
