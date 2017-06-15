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

// FuncDecl represents function declaration
type FuncDecl struct {
	Name    string
	Params  []Parameter
	Results []Type
	// TODO body
}

func (fd FuncDecl) declMarker() {}

// Parameter represents function parameter
type Parameter string

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
	fmt.Fprint(&out, fd.Name)
	fmt.Fprint(&out, typeSeparator)
	fmt.Fprint(&out, Parameters(fd.Params))
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, arrow)
	}
	fmt.Fprint(&out, Results(fd.Results), lf)

	if fd.Results == nil {
		fmt.Fprintf(&out, "%s = %s\n", fd.Name, undefined)
	}
	// TODO implement body

	return out.String()
}

// Parameters represents function parameters
type Parameters []Parameter // TODO not only types

func (ps Parameters) String() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i])
	}
	return strings.Join(ss, arrow)
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
		return "(" + strings.Join(ss, ", ") + ")"
	}
}
