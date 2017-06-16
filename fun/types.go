// Package fun contains Fun language AST and means to prettyprint its source code.
package fun

import (
	"bytes"
	"fmt"
	"strings"
)

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

// Decl represents declaration.
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

func (u Undef) String() string {
	return undefined
}

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

func (imp Import) String() string {
	if imp.Alias == "" {
		return fmt.Sprintf("import \"%s\"", imp.Path)
	}
	return fmt.Sprintf("import \"%s\" as \"%s\"", imp.Path, imp.Alias)
}

func (mod Module) String() string {
	topLevels := make([]string, 1+1+len(mod.Decls))

	// Module name
	topLevels[0] = fmt.Sprintf("module %s where\n", mod.Name)

	// Imports
	imports := make([]string, len(mod.Imports))
	for i, imp := range mod.Imports {
		imports[i] = imp.String()
	}
	topLevels[1] = strings.Join(imports, lf) + lf

	// Top-level declarations
	for i, decl := range mod.Decls {
		topLevels[2+i] = fmt.Sprint(decl)
	}

	return strings.Join(topLevels, lf) + lf
}

func (fd FuncDecl) String() string {
	var out bytes.Buffer
	// Type signature
	fmt.Fprint(&out, fd.Name)
	fmt.Fprint(&out, typeSeparator)
	fmt.Fprint(&out, fd.Params)
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, arrow)
	}
	fmt.Fprint(&out, fd.Results, lf)

	// Name and parameters
	fmt.Fprint(&out, fd.Name)
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, space, fd.Params.Names())
	}
	fmt.Fprint(&out, binding)

	// TODO implement body
	if fd.Body == nil {
		fmt.Fprint(&out, undefined)
	} else {
		fmt.Fprint(&out, fd.Body)
	}

	return out.String() + lf
}

// Parameters represents function parameters.
type Parameters []Parameter // TODO not only types

func (ps Parameters) String() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Type)
	}
	return strings.Join(ss, arrow)
}

// Names build parameter list for binding.
func (ps Parameters) Names() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Name)
	}
	return strings.Join(ss, space)
}

// Results represents function result list.
type Results []Type

func (ts Results) String() string {
	switch len(ts) {
	case 0:
		return unit
	case 1:
		return string(ts[0])
	default:
		ss := make([]string, len(ts))
		for i := 0; i < len(ts); i++ {
			ss[i] = string(ts[i])
		}
		return openBracket + strings.Join(ss, comma) + closeBracket
	}
}

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

func (fa FuncApplication) String() string {
	var buf bytes.Buffer
	if fa.Module != "" {
		fmt.Fprint(&buf, fa.Module, dot)
	}
	fmt.Fprint(&buf, fa.Name)
	if len(fa.Arguments) > 0 {
		args := make([]string, len(fa.Arguments))
		for i := 0; i < len(args); i++ {
			switch arg := fa.Arguments[i].(type) {
			case FuncApplication:
				args[i] = fmt.Sprintf("(%s)", arg)
			default:
				args[i] = fmt.Sprint(arg)
			}
		}
		fmt.Fprint(&buf, space, strings.Join(args, space))
	}
	return buf.String()
}

// Assuming non-empty body, empty do block does not really makes sense.
func (do DoBlock) String() string {
	buf := bytes.NewBufferString(doDecl)
	for _, line := range do.Text {
		fmt.Fprint(buf, intendation, line, lf)
	}
	return strings.TrimSuffix(buf.String(), lf)
}

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

func (t String) String() string {
	return fmt.Sprintf("%#v", t)
}

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

func (op InfixOperation) String() string {
	return fmt.Sprintf("%s %s %s", op.X, op.Operator, op.Y)
}

// Operator represents binary operator
type Operator string

// Tuple represents a group of values
type Tuple []Expression

func (t Tuple) String() string {
	ss := make([]string, len(t))
	for i := 0; i < len(t); i++ {
		ss[i] = fmt.Sprint(t[i])
	}
	return openBracket + strings.Join(ss, comma) + closeBracket
}
