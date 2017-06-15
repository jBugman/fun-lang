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
)

// Module represents single source file
type Module struct {
	Name    string
	Imports []Import
	Decls   []Decl
}

// Import represents import
type Import struct {
	Path  string
	Alias string
}

// Decl represents declaration
type Decl interface {
	String() string
	declMarker()
}

func (fd FuncDecl) declMarker() {}

// FuncBody represents function body
type FuncBody interface {
	String() string
	funcBodyMarker()
}

// FuncDecl represents function declaration
type FuncDecl struct {
	Name    string
	Params  Parameters
	Results Results
	Body    FuncBody
}

// Parameter represents function parameter
type Parameter struct {
	Name string
	Type Type
}

// Type represents type
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
		topLevels[2+i] = decl.String()
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
	}

	return out.String() + lf
}

// Parameters represents function parameters
type Parameters []Parameter // TODO not only types

func (ps Parameters) String() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Type)
	}
	return strings.Join(ss, arrow)
}

// Names build parameter list for binding
func (ps Parameters) Names() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Name)
	}
	return strings.Join(ss, space)
}

// Results represents function result list
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
		return "(" + strings.Join(ss, comma) + ")"
	}
}
