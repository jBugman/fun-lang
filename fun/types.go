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

// Decl represents top-level declaration.
type Decl interface {
	declMarker()
}

func (fd FuncDecl) declMarker() {}

// FuncBody represents function body.
type FuncBody interface {
	funcBodyMarker()
}

func (fa FuncApplication) funcBodyMarker() {}
func (do DoBlock) funcBodyMarker()         {}
func (u Undef) funcBodyMarker()            {}

// Undef represents function body placeholder.
type Undef bool

// Undefined is an Undef singleton.
const Undefined Undef = true

// FuncDecl represents function declaration.
type FuncDecl struct {
	Name    string
	Params  Parameters
	Results Results
	Body    FuncBody
}

// Parameter represents function parameter.
type Parameter struct {
	Name string
	Type Type
}

// Type represents type.
type Type string

// Parameters represents function parameters.
type Parameters []Parameter // TODO not only types

// Results represents function result list.
type Results []Type

// Expression is a pure function.
type Expression interface {
	funcBodyMarker()
	// expressionMarker()
}

// Statement just performs side effects.
type Statement interface {
	// TODO looks like it will be used to describe syntax constructs
	// but all 'statements' are really expressions
	funcBodyMarker()
	// statementMarker()
}

// DoBlock represents raw Go code as a function body.
type DoBlock struct {
	Text []string
}

// FuncApplication represents function application.
type FuncApplication struct {
	Name      string
	Module    string
	Arguments []Argument
	Kind      funcApplicationKind
}

type funcApplicationKind uint8

// Kinds of a FuncApplication
const (
	EXPRESSION funcApplicationKind = iota
	STATEMENT  funcApplicationKind = iota
)

// Argument represents argument to which a function is applied.
type Argument interface {
	argumentMarker()
}

// Literal represents language literals.
type Literal interface {
	argumentMarker()
	literalMarker()
}

func (fa FuncApplication) argumentMarker() {}

// Int maps to Go int.
type Int string

func (t Int) literalMarker()  {}
func (t Int) argumentMarker() {}

// Float maps to Go float32.
type Float string

func (t Float) literalMarker()  {}
func (t Float) argumentMarker() {}

// Double maps to Go float64.
type Double string

func (t Double) literalMarker()  {}
func (t Double) argumentMarker() {}

// String wraps Go string.
type String string

func (t String) literalMarker()  {}
func (t String) argumentMarker() {}

// Bool maps to Go bool.
type Bool string

func (t Bool) literalMarker()  {}
func (t Bool) argumentMarker() {}

// Char maps to Go char.
type Char string

func (t Char) literalMarker()  {}
func (t Char) argumentMarker() {}

// Imaginary maps to Go imaginary double.
type Imaginary string

func (t Imaginary) literalMarker()  {}
func (t Imaginary) argumentMarker() {}

// Var represents something passed by name.
type Var string

func (v Var) argumentMarker() {}

// InfixOperation represents
type InfixOperation struct {
	X, Y     Expression
	Operator Operator
}

func (op InfixOperation) funcBodyMarker() {}

// Operator represents binary operator
type Operator string

// Tuple represents a group of values
type Tuple []Expression
