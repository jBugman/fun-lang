// Package fun provides types of the Fun language.
package fun

import "github.com/jBugman/fun-lang/go/fun/code"

// Expr is a generic s-expression.
type Expr interface {
	Pos() code.Pos
}

// List is a list.
type List struct {
	pos code.Pos
	xs  []Expr
}

// NewList creates a List.
func NewList(xs []Expr, pos code.Pos) List {
	return List{
		pos: pos,
		xs:  xs,
	}
}

// Atom is an atom.
type Atom interface {
	Expr
	Val() interface{} // TODO: something more meaningful
}

// Ident is an identifier.
type Ident struct {
	pos code.Pos
	x   string
}

// NewIdent creates an Ident.
func NewIdent(val string, pos code.Pos) Ident {
	return Ident{
		pos: pos,
		x:   val,
	}
}

// StrLit is a string literal.
type StrLit struct {
	pos code.Pos
	x   string
}

// Pos designates membership of Expr.
func (x List) Pos() code.Pos { return x.pos }

// Pos designates membership of Expr.
func (x Ident) Pos() code.Pos { return x.pos }

// Pos designates membership of Expr.
func (x StrLit) Pos() code.Pos { return x.pos }

// Val designates membership of Atom.
func (x Ident) Val() interface{} { return x.x }

// Val designates membership of Atom.
func (x StrLit) Val() interface{} { return x.x }
