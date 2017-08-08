// Package parser provides a parser for the Fun language.
package parser

import (
	"strconv"
	"strings"
	"unicode"

	"github.com/pkg/errors"

	"github.com/jBugman/fun-lang/func-parse/fun"
	"github.com/jBugman/fun-lang/func-parse/fun/code"
)

var (
	keywords  = make(map[string]bool)
	operators = make(map[string]bool)
	escapable = make(map[rune]bool)
)

func init() {
	for _, s := range fun.Keywords {
		keywords[s] = true
	}
	for _, s := range fun.Operators {
		operators[s] = true
	}
	for _, s := range []rune{'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\''} {
		escapable[s] = true
	}
}

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

// ParseError annotates concrete error with its position in code.
type ParseError interface {
	error
	Pos() code.Pos
}

type parseError struct {
	pos code.Pos
	err error
}

func (pe parseError) Error() string {
	return pe.err.Error()
}

func (pe parseError) Pos() code.Pos {
	return pe.pos
}

func errorf(pos code.Pos, format string, args ...interface{}) ParseError {
	return parseError{
		pos: pos,
		err: errors.Errorf(format, args...),
	}
}

func unexpected(pos code.Pos, c rune, expected string) ParseError {
	var s = string(c)
	if c == '\n' {
		s = "\\n"
	}
	if c == 0 {
		s = "EOF"
	} else {
		s = "'" + s + "'"
	}
	return errorf(pos, "expected %s, found %s", expected, s)
}

// Parse parses source code as an expression.
func Parse(src []byte) (fun.Expr, ParseError) {
	source := strings.TrimRightFunc(string(src), unicode.IsSpace)
	sc := newScanner(source)
	return parseOneExpression(sc)
}

func skipSpace(sc scanner) scanner {
	var comment bool
	for {
		c, err := sc.next()
		switch {

		case err != nil:
			return sc

		// comments
		case c == ';':
			sc.commit()
			comment = true

		case comment && c == '\n':
			sc.commit()
			return sc

		case comment:
			sc.commit()

		case unicode.IsSpace(c):
			sc.commit()

		default:
			return sc
		}
	}
}

func parseOneExpression(sc scanner) (fun.Expr, ParseError) {
	if len(sc.source) == 0 {
		return nil, unexpected(sc.pos, 0, "expression")
	}
	x, sc, err := parseExpression(sc)
	// Check if all input bar including whitespace is consumed
	if err == nil && sc.cursor < len(sc.source) {
		err = unexpected(sc.pos, sc.c, "EOF")
	}
	return x, err
}

func parseExpression(sc scanner) (fun.Expr, scanner, ParseError) {
	var x fun.Expr
	var err ParseError

	sc = skipSpace(sc)

	x, sc, err = parseAtom(sc)
	if err == nil {
		sc = skipSpace(sc)
		return x, sc, nil
	}

	x, sc, err = parseList(sc)
	if err == nil {
		sc = skipSpace(sc)
		return x, sc, nil
	}

	sc = skipSpace(sc)
	return nil, sc, err
}

func parseAtom(sc scanner) (fun.Atom, scanner, ParseError) {
	var err ParseError
	var x fun.Atom
	// Numeric
	x, sc, err = parseNumber(sc)
	if err == nil {
		return x, sc, nil
	}
	// String
	x, sc, err = parseString(sc)
	if err == nil {
		return x, sc, nil
	}
	// Char
	x, sc, err = parseChar(sc)
	if err == nil {
		return x, sc, nil
	}
	// Operator
	x, sc, err = parseOperator(sc)
	if err == nil {
		return x, sc, nil
	}
	// Type
	x, sc, err = parseType(sc)
	if err == nil {
		return x, sc, nil
	}
	// Ident, Keyword or Bool
	var id fun.Ident
	id, sc, err = parseIdent(sc)
	if err == nil {
		_, ok := keywords[id.X]
		switch {
		case id.X == "true":
			return fun.Bool{X: true, Pos: id.Pos}, sc, nil
		case id.X == "false":
			return fun.Bool{X: false, Pos: id.Pos}, sc, nil
		case ok:
			return fun.Keyword{X: id.X, Pos: id.Pos}, sc, nil
		}
		return id, sc, nil
	}
	return nil, sc, unexpected(sc.pos, sc.c, "atom")
}

func parseIdent(sc scanner) (fun.Ident, scanner, ParseError) {
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

func parseOperator(sc scanner) (fun.Operator, scanner, ParseError) {
	var val string
	var start = sc
	for {
		c, _ := sc.next()
		switch {

		case unicode.IsSymbol(c) || unicode.IsPunct(c):
			sc.commit()
			val += string(c)

		default:
			_, ok := operators[val]
			if !ok {
				return fun.Operator{}, start, unexpected(sc.pos, c, "operator")
			}
			return fun.Operator{X: val, Pos: start.pos}, sc, nil
		}
	}
}

func parseList(sc scanner) (fun.List, scanner, ParseError) {
	var xs []fun.Expr
	var start = sc
	var opened = false
	for {
		c, err := sc.next()
		switch {

		case err != nil:
			return fun.LL(xs, start.pos), sc, nil

		case isSkippable(c):
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
				return fun.List{}, sc, err.(ParseError)
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

// TODO: parse \u sequences and like
func parseChar(sc scanner) (fun.Char, scanner, ParseError) {
	const tick = '\''
	var val string
	var start = sc
	for {
		c, _ := sc.next()
		switch val {

		case "":
			if c == tick {
				sc.commit()
				val += string(c)
			} else {
				return fun.Char{}, start, unexpected(sc.pos, c, "char literal")
			}

		case "'":
			if c == tick {
				return fun.Char{}, start, unexpected(sc.pos, c, "char literal")
			}
			sc.commit()
			val += string(c)

		case "'\\":
			if isEscape(c) {
				sc.commit()
				val += string(c)
			} else {
				return fun.Char{}, start, unexpected(sc.pos, c, "char literal")
			}

		default:
			if c == tick {
				sc.commit()
				return fun.Char{
					X:   val[1:], // Trimming '
					Pos: start.pos,
				}, sc, nil
			}
			return fun.Char{}, start, unexpected(sc.pos, c, "'\\''")
		}
	}
}

func parseString(sc scanner) (fun.String, scanner, ParseError) {
	var val string
	var start = sc
	var raw bool
	for {
		c, err := sc.next()
		switch {

		case len(val) == 0:
			switch c {

			case '"':
				sc.commit()
				val += string(c)

			case '`':
				sc.commit()
				val += string(c)
				raw = true

			default:
				return fun.String{}, start, unexpected(sc.pos, c, "string")
			}

		case strings.HasSuffix(val, "\\"):
			if isEscape(c) || c == '"' {
				sc.commit()
				val += string(c)
			} else {
				return fun.String{}, start, unexpected(sc.pos, c, "escape sequence")
			}

		case c == '"' && !raw:
			sc.commit()
			return fun.String{
				X:   val[1:], // trimming "
				Pos: start.pos,
			}, sc, nil

		case c == '`' && raw:
			sc.commit()
			return fun.String{
				X:   val[1:], // trimming `
				Pos: start.pos,
				Raw: true,
			}, sc, nil

		case c == '\n' && !raw:
			return fun.String{}, start, unexpected(sc.pos, c, "'\"'")

		case err != nil:
			return fun.String{}, start, unexpected(sc.pos, 0, "'\"'")

		default:
			sc.commit()
			val += string(c)
		}
	}
}

func parseType(sc scanner) (fun.Type, scanner, ParseError) {
	var start = sc
	c, _ := sc.next()
	if c != ':' {
		return fun.Type{}, start, unexpected(sc.pos, c, "':'")
	}

	sc.commit()
	id, sc, err := parseIdent(sc)
	if err != nil {
		return fun.Type{}, start, unexpected(sc.pos, c, "type")
	}
	return fun.Type{X: id.X, Pos: start.pos}, sc, nil
}

func parseNumber(sc scanner) (fun.Atom, scanner, ParseError) {
	var start = sc
	var val string
	var stop bool

	for !stop {
		c, err := sc.next()
		switch {

		case err != nil:
			stop = true

		case c == ')':
			stop = true

		case unicode.IsSpace(c):
			stop = true

		default:
			sc.commit()
			val += string(c)
		}
	}

	switch {

	case strings.HasPrefix(val, "0x"):
		x, err := strconv.ParseInt(val, 0, 0)
		if err != nil {
			return nil, start, parseError{
				pos: start.pos,
				err: errors.Wrap(err, "expected hex literal"),
			}
		}
		return fun.Integer{X: int(x), Base: 16, Pos: start.pos}, sc, nil

	case val == "0":
		return fun.Integer{X: 0, Pos: start.pos}, sc, nil

	case strings.ContainsAny(val, ".e"):
		x, err := strconv.ParseFloat(val, 64)
		if err != nil {
			return nil, start, parseError{
				pos: start.pos,
				err: errors.Wrap(err, "expected float literal"),
			}
		}
		return fun.Double{X: x, Pos: start.pos}, sc, nil

	case strings.HasPrefix(val, "0"):
		x, err := strconv.ParseInt(val, 0, 0)
		if err != nil {
			return nil, start, parseError{
				pos: start.pos,
				err: errors.Wrap(err, "expected octal literal"),
			}
		}
		return fun.Integer{X: int(x), Base: 8, Pos: start.pos}, sc, nil

	default:
		x, err := strconv.ParseInt(val, 10, 0)
		if err != nil {
			return nil, start, parseError{
				pos: start.pos,
				err: errors.Wrap(err, "expected int literal"),
			}
		}
		return fun.Integer{X: int(x), Pos: start.pos}, sc, nil
	}
}

func isSkippable(x rune) bool {
	return unicode.IsSpace(x) || x == ';'
}

func isEscape(x rune) bool {
	_, ok := escapable[x]
	return ok
}
