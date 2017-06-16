// Package fun contains Fun language AST and means to prettyprint its source code.
package fun

const (
	lf            = "\n"
	undefined     = "undefined"
	arrow         = " -> "
	typeSeparator = " :: "
	unit          = "()"
	space         = " "
	comma         = ", "
	binding       = " = "
	dot           = "."
	intendation   = "    "
	doDecl        = "do" + lf
	openBracket   = "("
	closeBracket  = ")"
)

// Type represents type.
type Type string

// Module represents single source file.
type Module struct {
	Name    string
	Imports []Import
	Decls   []Decl
}

// Import represents import.
type Import struct {
	Path  string
	Alias string
}

/*** Top-level declarations ***/

// Decl represents top-level declaration.
type Decl interface {
	declMarker()
}

// FuncDecl represents function declaration.
type FuncDecl struct {
	Name    string
	Params  Parameters
	Results Results
	Body    FuncBody
}

func (fd FuncDecl) declMarker() {}

/*** Function declaration ***/

// Parameter represents function parameter.
type Parameter struct {
	Name string
	Type Type
}

// Parameters represents function parameters.
type Parameters []Parameter

// Results represents function result list.
type Results []Type

// FuncBody represents function body.
type FuncBody interface {
	funcBodyMarker()
}

/*** Function body ***/

// Undef represents function body placeholder.
type Undef bool

func (u Undef) funcBodyMarker()   {}
func (u Undef) expressionMarker() {}

// Undefined is an Undef singleton.
const Undefined Undef = true

// DoBlock represents raw Go code as a function body.
type DoBlock struct {
	Text []string
}

func (do DoBlock) funcBodyMarker() {}

// SingleExprBody represents sungle expression as a function body.
type SingleExprBody struct {
	Expr Expression
}

func (b SingleExprBody) funcBodyMarker() {}

// Expression is something that has value
type Expression interface {
	expressionMarker()
}

/*** Expresstions ***/

// FuncApplication represents function application.
type FuncApplication struct {
	Func      FunctionVal
	Arguments []Expression
}

func (fa FuncApplication) expressionMarker() {}

// FunctionVal represents function addressed by name.
type FunctionVal struct {
	Name   string
	Module string
}

func (fa FunctionVal) expressionMarker() {}

// InfixOperation represents
type InfixOperation struct {
	X, Y     Expression
	Operator Operator
}

func (op InfixOperation) funcBodyMarker() {}

// Tuple represents a group of values
type Tuple []Expression

func (t Tuple) expressionMarker() {}

// Operator represents binary operator
type Operator string

// Val represents something passed by name.
type Val string

func (v Val) argumentMarker()   {}
func (v Val) expressionMarker() {}

/*** Literals ***/

// Literal represents language literals.
type Literal interface {
	Expression
	literalMarker()
}

// Int maps to Go int.
type Int string

func (t Int) literalMarker()    {}
func (t Int) expressionMarker() {}

// Float maps to Go float32.
type Float string

func (t Float) literalMarker()    {}
func (t Float) expressionMarker() {}

// Double maps to Go float64.
type Double string

func (t Double) literalMarker()    {}
func (t Double) expressionMarker() {}

// String wraps Go string.
type String string

func (t String) literalMarker()    {}
func (t String) expressionMarker() {}

// Bool maps to Go bool.
type Bool string

func (t Bool) literalMarker()    {}
func (t Bool) expressionMarker() {}

// Char maps to Go char.
type Char string

func (t Char) literalMarker()    {}
func (t Char) expressionMarker() {}

// Imaginary maps to Go imaginary double.
type Imaginary string

func (t Imaginary) literalMarker()    {}
func (t Imaginary) expressionMarker() {}
