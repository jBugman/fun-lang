package fun

import (
	"github.com/jBugman/fun-lang/func-parse/fun/code"
)

// Type shortcuts

// L creates List from a Expr vararg.
func L(pos code.Pos, xs ...Expr) List {
	return List{XS: xs, Pos: pos}
}

// LL creates List from a slice of Exprs.
func LL(xs []Expr, pos code.Pos) List {
	return List{XS: xs, Pos: pos}
}

// ID creates Ident.
func ID(x string, pos code.Pos) Ident {
	return Ident{X: x, Pos: pos}
}

// KW creates Keyword.
func KW(x string, pos code.Pos) Keyword {
	return Keyword{X: x, Pos: pos}
}

// OP creates Operator.
func OP(x string, pos code.Pos) Operator {
	return Operator{X: x, Pos: pos}
}

// TP creates Type.
func TP(x string, pos code.Pos) Type {
	return Type{X: x, Pos: pos}
}

// SL creates String.
func SL(x string, pos code.Pos) String {
	return String{X: x, Pos: pos}
}

// CL creates Char.
func CL(x string, pos code.Pos) Char {
	return Char{X: x, Pos: pos}
}

// IL creates Integer.
func IL(x int, pos code.Pos) Integer {
	return Integer{X: x, Pos: pos}
}

// DL creates Double.
func DL(x float64, pos code.Pos) Double {
	return Double{X: x, Pos: pos}
}

// BL creates Bool.
func BL(x bool, pos code.Pos) Bool {
	return Bool{X: x, Pos: pos}
}

// OL creates Oct.
func OL(x int, pos code.Pos) Oct {
	return Oct{X: x, Pos: pos}
}

// HL creates Hex.
func HL(x int, pos code.Pos) Hex {
	return Hex{X: x, Pos: pos}
}
