// Package fun contains Fun language AST.
package fun

// Package represents single source file.
type Package struct {
	Name      string
	Imports   []Import
	TopLevels []TopLevel
}

// Import represents import.
type Import struct {
	Path  string
	Alias string
}

/* TopLevel */

// TopLevel represents top-level declaration.
type TopLevel interface {
	isTopLevel()
}

// FuncDecl represents function declaration.
type FuncDecl struct {
	Name    string
	Params  []Param
	Results []Type
	Body    FuncBody
}

func (fd FuncDecl) isTopLevel() {}

/* Var */

// Var represents variable
type Var string // TODO move to Expr

func (v Var) isExpr() {}

/* Type */

// Type represents type.
type Type interface {
	isType()
}

// Atomic represents basic type.
type Atomic struct {
	V string
}

func (t Atomic) isType() {}

// Supported atomic type singletons
var (
	IntT    = Atomic{V: "int"}
	DoubleT = Atomic{V: "double"}
	CharT   = Atomic{V: "char"}
	StringT = Atomic{V: "string"}
)

// Slice represents Go slice.
type Slice struct {
	V Type
}

func (t Slice) isType() {}

// Map represents a Go map.
type Map struct {
	K, V Type
}

func (t Map) isType() {}

/* Field */

// Field is a variable name with its type.
type Field struct {
	Name string
	Type Type
}

/* Param */

// Param represents function parameter.
type Param struct {
	V Field
}

// NewParam creates new Param instance.
func NewParam(name, t string) Param {
	return Param{Field{Name: name, Type: Atomic{V: t}}}
}

/* FuncBody */

// FuncBody represents function body.
type FuncBody interface {
	isFuncBody()
}

/*** Function body ***/

// Undefined represents function body placeholder.
type Undefined bool

func (b Undefined) isFuncBody() {}
func (b Undefined) isExpr()     {}

// Undef is an Undefined singleton.
const Undef Undefined = true

// Single represents sungle expression as a function body.
type Single struct {
	Expr Expr
}

func (b Single) isFuncBody() {}

// Inline represents raw Go code as a function body.
type Inline struct {
	Block []string
}

func (b Inline) isFuncBody() {}

/* FuncName */

// FuncName represents function addressed by name.
type FuncName struct {
	V string
	// TODO split it back
	// Package string
	// Name    string
}

func (fa FuncName) isExpr() {}

/* Expr */

// Expr is something that can be reduced to terminal Expr or passed as value.
type Expr interface {
	isExpr()
}

// Application represents function application.
type Application struct {
	Name FuncName
	Args []Expr
}

func (e Application) isExpr() {}

// BinaryOp represents infix binary operator.
type BinaryOp struct {
	X, Y Expr
	Op   Operator
}

func (e BinaryOp) isExpr() {}

// Operator represents binary operator.
type Operator string

// Literal represents language literals.
type Literal interface {
	Expr
	isLiteral()
}

// Results represents a group of expressions to return from function.
type Results []Expr

func (e Results) isExpr() {}

/* ForHeader */
// TODO add

/* Literal */

// StringLit is a literal Go string.
type StringLit struct {
	V string
}

func (l StringLit) isLiteral() {}
func (l StringLit) isExpr()    {}

// CharLit is a literal Go char.
type CharLit struct {
	V rune
}

func (l CharLit) isLiteral() {}
func (l CharLit) isExpr()    {}

// IntegerLit is a literal Go int.
type IntegerLit struct {
	V int
}

func (l IntegerLit) isLiteral() {}
func (l IntegerLit) isExpr()    {}

// DoubleLit is a literal Go floating value (specifically float64).
type DoubleLit struct {
	V float64
}

func (l DoubleLit) isLiteral() {}
func (l DoubleLit) isExpr()    {}

// BoolLit is a literal Go bool.
type BoolLit struct {
	V bool
}

func (l BoolLit) isLiteral() {}
func (l BoolLit) isExpr()    {}

// HexLit is a literal Go uint written as hex value.
type HexLit struct {
	V uint
}

func (l HexLit) isLiteral() {}
func (l HexLit) isExpr()    {}

// Float maps to Go float32.
// type Float string
// func (t Float) literalMarker()    {}
// func (t Float) expressionMarker() {}

// // Imaginary maps to Go imaginary double.
// type Imaginary string
// func (t Imaginary) literalMarker()    {}
// func (t Imaginary) expressionMarker() {}
