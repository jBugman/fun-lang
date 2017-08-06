package parser

import (
	"fmt"
	"unicode"

	"github.com/pkg/errors"
)

// Pos represents position in code.
type Pos struct {
	Line int
	Col  int
}

// Expr is a s-expression.
type Expr interface {
	Pos() Pos
}

// List is a list.
type List struct {
	pos Pos
	xs  []Expr
}

// Atom is an atom.
type Atom interface {
	Expr
	Val() string
}

// Ident is an identifier.
type Ident struct {
	pos Pos
	x   string
}

// StrLit is a string literal.
type StrLit struct {
	pos Pos
	x   string
}

// Pos is a position.
func (x List) Pos() Pos { return x.pos }

// Pos is position.
func (x Ident) Pos() Pos { return x.pos }

// Val is an atom value.
func (x Ident) Val() string { return x.x }

// Pos is position..
func (x StrLit) Pos() Pos { return x.pos }

// Val is a atom value.
func (x StrLit) Val() string { return x.x }

type scanner struct {
	source []rune
	pos    Pos
	cursor int
	c      rune
}

func newScanner(text string) scanner {
	return scanner{
		source: []rune(text),
		pos:    Pos{1, 1},
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

type pe struct {
	pos Pos
	err error
}

func errorf(pos Pos, format string, args ...interface{}) error {
	return pe{
		pos: pos,
		err: errors.Errorf(format, args...),
	}
}

func unexpected(pos Pos, c rune, expected string) error {
	var s = string(c)
	if c == '\n' {
		s = "\\n"
	}
	return errorf(pos, "expected %s, found '%s'", expected, s)
}

func (e pe) Error() string {
	return fmt.Sprintf("%d:%d: %s", e.pos.Line, e.pos.Col, e.err)
}

// Parse parses source code as an expression.
func Parse(src []byte) (Expr, error) {
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

func parseOneExpression(sc scanner) (Expr, error) {
	x, sc, err := parseExpression(sc)
	// Check if all input is consumed
	if err == nil && sc.cursor < len(sc.source) {
		err = unexpected(sc.pos, sc.c, "'EOF'")
	}
	return x, err
}

func parseExpression(sc scanner) (Expr, scanner, error) {
	var x Expr
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

func parseAtom(sc scanner) (Atom, scanner, error) {
	var x Atom
	var err error
	x, sc, err = parseIdent(sc)
	if err != nil {
		return nil, sc, err
	}
	return x, sc, nil
}

func parseIdent(sc scanner) (Ident, scanner, error) {
	// TODO: proper ident
	var val string
	var start = sc
	for {
		c, err := sc.next()
		switch {
		case err != nil:
			return Ident{pos: start.pos, x: val}, sc, nil
		case unicode.IsLetter(c):
			sc.commit()
			val += string(c)
		default:
			if len(val) == 0 {
				return Ident{}, start, unexpected(sc.pos, c, "letter") // FIXME: letter
			}
			return Ident{pos: start.pos, x: val}, sc, nil
		}
	}
}

func parseList(sc scanner) (List, scanner, error) {
	var xs []Expr
	var start = sc
	var opened = false
	for {
		c, err := sc.next()
		switch {

		case err != nil:
			return List{pos: start.pos, xs: xs}, sc, nil

		case unicode.IsSpace(c):
			sc = skipSpace(sc)

		case c == '(':
			if !opened {
				sc.commit()
				opened = true
				break
			}

			var x List
			x, sc, err = parseList(sc)
			if err != nil {
				return List{}, sc, err
			}
			xs = append(xs, x)

		case c == ')':
			if opened {
				sc.commit()
				return List{pos: start.pos, xs: xs}, sc, nil
			}
			return List{}, start, unexpected(sc.pos, c, "expression")

		case !opened:
			return List{}, start, unexpected(sc.pos, c, "list")

		default:
			var x Expr
			x, sc, err = parseAtom(sc)
			if err == nil {
				xs = append(xs, x)
				break
			}
			return List{}, start, unexpected(sc.pos, c, "atom")
		}
	}
}
