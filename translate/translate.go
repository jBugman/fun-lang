package translate

import (
	"bytes"
	"fmt"
	"go/ast"
	"strings"
)

const (
	unit      = "()"
	undefined = "undefined"
)

// Module represents single source file
type Module interface {
	PrettyPrint() string
}

type module struct {
	*ast.File
}

// FromAST converts Go ast.File to Fun module
func FromAST(src *ast.File) Module {
	return module{src}
}

type importStmt struct {
	*ast.ImportSpec
}

func (i importStmt) String() string {
	return i.ImportSpec.Path.Value
}

type ident struct {
	*ast.Ident
}

func (i ident) String() string {
	return i.Name
}

type funcDecl struct {
	*ast.FuncDecl
}

func (fd funcDecl) String() string {
	var out bytes.Buffer
	name := fd.FuncDecl.Name
	fmt.Fprintf(&out, "%s :: ", name)
	params := parameterList{fd.Type.Params}.String()
	if params != "" {
		fmt.Fprintf(&out, "%s -> ", params)
	}
	result := resultList{fd.Type.Results}.String()
	fmt.Fprintf(&out, "%s\n", result)

	fmt.Fprintf(&out, "%s = %s\n", name, undefined) // TODO implement body

	return out.String()
}

func types(fl *ast.FieldList) []string {
	if fl == nil {
		return []string{}
	}
	fs := fl.List
	types := make([]string, len(fs))
	for i, field := range fs {
		switch t := field.Type.(type) {
		case *ast.Ident:
			types[i] = t.Name
		}
	}
	return types
}

type parameterList struct {
	*ast.FieldList
}

func (fl parameterList) String() string {
	ts := types(fl.FieldList)
	return strings.Join(ts, " -> ")
}

type resultList struct {
	*ast.FieldList
}

func (fl resultList) String() string {
	ts := types(fl.FieldList)
	switch len(ts) {
	case 0:
		return unit
	case 1:
		return ts[0]
	default:
		return "(" + strings.Join(ts, ", ") + ")"
	}
}

// PrettyPrint prints File as Fun source code
func (m module) PrettyPrint() string {
	var out bytes.Buffer

	// Module name
	fmt.Fprintf(&out, "module %s where\n\n", strings.Title(ident{m.File.Name}.String()))

	// Imports
	if len(m.File.Imports) > 0 {
		for _, imp := range m.File.Imports {
			fmt.Fprintf(&out, "import %s\n", importStmt{imp}.String())
		}
		fmt.Fprintln(&out)
	}

	// Top-level declarations
	for _, decl := range m.File.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			fun := funcDecl{d}
			fmt.Fprintln(&out, fun)
		}
	}

	return out.String()
}
