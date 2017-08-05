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

// type parser struct {
// 	pos Pos
// }

func parseSource(src []byte) (Expr, error) {
	var source = []rune(string(src))
	var pos = Pos{Line: 1, Col: 1}
	var cursor int
	// var backtracker int
	// var c rune
	return parse(source, pos, cursor, NOTHING)
}

func parse(source []rune, startPos Pos, cursor int, state state) (Expr, error) {
	fmt.Println(string(source))
	var val string
	var pos = Pos{Line: 1, Col: 1}
	var backtracker = cursor
	var c rune
	for cursor < len(source) {
		c = source[cursor]
		fmt.Println(string(c), pos, state)
		cursor++
		backtracker++

		switch {
		case state == NOTHING:
			switch {
			case c == ';':
				// state = COMMENT
				parse(source[cursor:], pos, 0, COMMENT)
			case c == '(':
				parse(source[cursor:], pos, cursor, LIST)
			case c == '"':
				parse(source[cursor:], pos, cursor, STRLIT)
			case unicode.IsLetter(c):
				parse(source[cursor:], pos, cursor, IDENT)
				// val += string(c)
			}
		// case state == LIST:
		case state == IDENT:
			switch {
			case unicode.IsLetter(c):
				val += string(c)
			case unicode.IsSpace(c):
				// state
			}
		}

		// Advance Pos
		// pos.Col++
		// if c == '\n' {
		// 	pos.Col = 1
		// 	pos.Line++
		// }
	}

	return nil, errors.New("not implemented")
}

type scanner struct {
	source []rune
	pos    Pos
	cursor int
	c      rune
}

func newScanner(text []rune) scanner {
	return scanner{
		source: text,
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
		case unicode.IsSpace(c):
			return Ident{pos: start.pos, x: val}, sc, nil
		default:
			return Ident{}, start, errors.Errorf("unexpected '%c', expected letter", c)
		}
	}
}
