package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"unicode"

	"github.com/pkg/errors"
)

func main() {
	filename := os.Args[1]
	fmt.Println(filename)
	source, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Fprintln(os.Stderr, errors.Wrap(err, "read file"))
		os.Exit(1)
	}
	result, err := parseSource(source)
	if err != nil {
		fmt.Fprintln(os.Stderr, errors.Wrap(err, "parse"))
		os.Exit(1)
	}

	fmt.Println(result)
}

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

type state string

// Current Parser state
const (
	NOTHING state = "nothing"
	COMMENT state = "comment"
	LIST    state = "list"
	IDENT   state = "ident"
	STRLIT  state = "string"
)

func parseSource(src []byte) (Expr, error) {
	var source = newScanner(string(src))
	result, scan, err := parseList(source)
	if err != nil {
		return nil, errors.Wrapf(err, "%d:%d", scan.pos.Line, scan.pos.Col)
	}
	return result, nil
}

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

func parseIdent(sc scanner) (Ident, scanner, error) {
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
				return Ident{}, start, errors.Errorf("unexpected '%c', expected letter", c)
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
			return List{}, start, errors.Errorf("unexpected '%c', expected expression", c)

		case !opened:
			return List{}, start, errors.Errorf("unexpected '%c', expected list", c)

		default:
			var x Expr
			x, sc, err = parseIdent(sc)
			if err == nil {
				xs = append(xs, x)
				break
			}
			return List{}, start, errors.Errorf("unexpected '%c', expected expression", c)
		}
	}
}
