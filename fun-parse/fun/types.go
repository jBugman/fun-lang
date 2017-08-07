// Package fun provides types of the Fun language.
package fun

import "github.com/jBugman/fun-lang/fun-parse/fun/code"

// Expr is a generic s-expression.
type Expr interface {
	exprMarker()
}

// Atom is an atom.
type Atom interface {
	Expr
	atomMarker()
}

// List is a list.
type List struct {
	Pos code.Pos
	XS  []Expr
}

func (List) exprMarker() {}

// Ident is an identifier.
type Ident struct {
	Pos code.Pos
	X   string
}

func (Ident) exprMarker() {}
func (Ident) atomMarker() {}

// Keyword is a language keyword.
type Keyword struct {
	Pos code.Pos
	X   string
}

func (Keyword) exprMarker() {}
func (Keyword) atomMarker() {}

// Operator is an operator.
type Operator struct {
	Pos code.Pos
	X   string
}

func (Operator) exprMarker() {}
func (Operator) atomMarker() {}

// Type is an identifier.
type Type struct {
	Pos code.Pos
	X   string
}

func (Type) exprMarker() {}
func (Type) atomMarker() {}

// String is a string literal.
type String struct {
	Pos code.Pos
	X   string
	Raw bool
}

func (String) exprMarker() {}
func (String) atomMarker() {}

// Char is a rune literal.
type Char struct {
	Pos code.Pos
	X   string
}

func (Char) exprMarker() {}
func (Char) atomMarker() {}

// Integer is an int literal.
type Integer struct {
	Pos code.Pos
	X   int
}

func (Integer) exprMarker() {}
func (Integer) atomMarker() {}

// Double is a float64 literal.
type Double struct {
	Pos code.Pos
	X   float64
}

func (Double) exprMarker() {}
func (Double) atomMarker() {}

// Oct is an octal int literal.
type Oct struct {
	Pos code.Pos
	X   int
}

func (Oct) exprMarker() {}
func (Oct) atomMarker() {}

// Hex is a hex int literal.
type Hex struct {
	Pos code.Pos
	X   int
}

func (Hex) exprMarker() {}
func (Hex) atomMarker() {}

// Bool is a boolean literal.
type Bool struct {
	Pos code.Pos
	X   bool
}

func (Bool) exprMarker() {}
func (Bool) atomMarker() {}
