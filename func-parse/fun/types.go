// Package fun provides types of the Fun language.
package fun

import "github.com/jBugman/fun-lang/func-parse/fun/code"

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
	Pos code.Pos `json:"pos"`
	XS  []Expr   `json:"xs"`
}

func (List) exprMarker() {}

// Ident is an identifier.
type Ident struct {
	Pos code.Pos `json:"pos"`
	X   string   `json:"x"`
}

func (Ident) exprMarker() {}
func (Ident) atomMarker() {}

// Keyword is a language keyword.
type Keyword struct {
	Pos code.Pos `json:"pos"`
	X   string   `json:"x"`
}

func (Keyword) exprMarker() {}
func (Keyword) atomMarker() {}

// Operator is an operator.
type Operator struct {
	Pos code.Pos `json:"pos"`
	X   string   `json:"x"`
}

func (Operator) exprMarker() {}
func (Operator) atomMarker() {}

// Type is an identifier.
type Type struct {
	Pos code.Pos `json:"pos"`
	X   string   `json:"x"`
}

func (Type) exprMarker() {}
func (Type) atomMarker() {}

// String is a string literal.
type String struct {
	Pos code.Pos `json:"pos"`
	X   string   `json:"x"`
	Raw bool     `json:"raw,omitempty"`
}

func (String) exprMarker() {}
func (String) atomMarker() {}

// Char is a rune literal.
type Char struct {
	Pos code.Pos `json:"pos"`
	X   string   `json:"x"`
}

func (Char) exprMarker() {}
func (Char) atomMarker() {}

// Integer is an int literal.
type Integer struct {
	Pos  code.Pos `json:"pos"`
	X    int      `json:"x"`
	Base int      `json:"base,omitempty"` // 0 means 10 as a default
}

func (Integer) exprMarker() {}
func (Integer) atomMarker() {}

// Double is a float64 literal.
type Double struct {
	Pos code.Pos `json:"pos"`
	X   float64  `json:"x"`
}

func (Double) exprMarker() {}
func (Double) atomMarker() {}

// Bool is a boolean literal.
type Bool struct {
	Pos code.Pos `json:"pos"`
	X   bool     `json:"x"`
}

func (Bool) exprMarker() {}
func (Bool) atomMarker() {}
