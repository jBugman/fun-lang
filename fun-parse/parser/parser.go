// Package parser provides a parser for the Fun language.
package parser

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/pkg/errors"

	"github.com/jBugman/fun-lang/fun-parse/fun"
	"github.com/jBugman/fun-lang/fun-parse/fun/code"
	// . "github.com/jBugman/fun-lang/fun-parse/fun/pattern"
)

type scanner struct {
	source []rune
	pos    code.Pos
	cursor int
	c      rune
}

func newScanner(text string) scanner {
	return scanner{
		source: []rune(text),
		pos:    code.Pos{Line: 1, Col: 1},
	}
}

func (s *scanner) next() (rune, error) {
	if s.cursor == len(s.source) {
		return 0, errors.New("EOF")
	}
	s.c = s.source[s.cursor]
	return s.c, nil
}

func (s *scanner) commit() {
	s.cursor++
	s.pos.Col++
	if s.c == '\n' {
		s.pos.Col = 1
		s.pos.Line++
	}
}

type parserError struct {
	pos code.Pos
	err error
}

func errorf(pos code.Pos, format string, args ...interface{}) error {
	return parserError{
		pos: pos,
		err: errors.Errorf(format, args...),
	}
}

func unexpected(pos code.Pos, c rune, expected string) error {
	var s = string(c)
	if c == '\n' {
		s = "\\n"
	}
	return errorf(pos, "expected %s, found '%s'", expected, s)
}

func (e parserError) Error() string {
	return fmt.Sprintf("%d:%d: %s", e.pos.Line, e.pos.Col, e.err)
}

// Parse parses source code as an expression.
func Parse(src []byte) (fun.Expr, error) {
	source := newScanner(string(src))
	trimmed := skipSpace(source)
	return parseOneExpression(trimmed)
}

func skipSpace(sc scanner) scanner {
	for {
		c, err := sc.next()
		switch {
		case err != nil:
			return sc
		case unicode.IsSpace(c):
			sc.commit()
		default:
			return sc
		}
	}
}

func parseOneExpression(sc scanner) (fun.Expr, error) {
	x, sc, err := parseExpression(sc)
	// Check if all input is consumed
	if err == nil && sc.cursor < len(sc.source) {
		err = unexpected(sc.pos, sc.c, "'EOF'")
	}
	return x, err
}

func parseExpression(sc scanner) (fun.Expr, scanner, error) {
	var x fun.Expr
	var err error

	x, sc, err = parseAtom(sc)
	if err == nil {
		return x, sc, nil
	}

	x, sc, err = parseList(sc)
	if err == nil {
		return x, sc, nil
	}

	return nil, sc, err
}

func parseAtom(sc scanner) (fun.Atom, scanner, error) {
	var x fun.Atom
	var err error
	x, sc, err = parseIdent(sc)
	if err != nil {
		return nil, sc, err
	}
	return x, sc, nil
}

func parseIdent(sc scanner) (fun.Ident, scanner, error) {
	var val string
	var start = sc
	for {
		c, err := sc.next()
		switch {
		case err != nil:
			return fun.ID(val, start.pos), sc, nil
		case c == '_':
			sc.commit()
			val += string(c)
		case c == '.' && len(val) > 0:
			sc.commit()
			val += string(c)
		case unicode.IsLetter(c):
			sc.commit()
			val += string(c)
		case unicode.IsDigit(c) && len(val) > 0:
			sc.commit()
			val += string(c)
		default:
			if len(val) == 0 {
				return fun.Ident{}, start, unexpected(sc.pos, c, "letter or '_'")
			}
			if strings.HasSuffix(val, ".") {
				return fun.Ident{}, start, unexpected(sc.pos, c, "letter")
			}
			return fun.ID(val, start.pos), sc, nil
		}
	}
}

func parseList(sc scanner) (fun.List, scanner, error) {
	var xs []fun.Expr
	var start = sc
	var opened = false
	for {
		c, err := sc.next()
		switch {

		case err != nil:
			return fun.LL(xs, start.pos), sc, nil

		case unicode.IsSpace(c):
			sc = skipSpace(sc)

		case c == '(':
			if !opened {
				sc.commit()
				opened = true
				break
			}

			var x fun.List
			x, sc, err = parseList(sc)
			if err != nil {
				return fun.List{}, sc, err
			}
			xs = append(xs, x)

		case c == ')':
			if opened {
				sc.commit()
				return fun.LL(xs, start.pos), sc, nil
			}
			return fun.List{}, start, unexpected(sc.pos, c, "expression")

		case !opened:
			return fun.List{}, start, unexpected(sc.pos, c, "list")

		default:
			var x fun.Expr
			x, sc, err = parseAtom(sc)
			if err == nil {
				xs = append(xs, x)
				break
			}
			return fun.List{}, start, unexpected(sc.pos, c, "atom")
		}
	}
}
