package translate

import (
	"fmt"
	"go/ast"
	"go/token"
	"strings"

	"../fun"
)

// FromFile converts Go ast.File to Fun Module
func FromFile(src *ast.File) (fun.Module, error) {

	var module fun.Module
	// Module name
	module.Name = strings.Title(identToString(src.Name))
	// Imports
	for _, imp := range src.Imports {
		funImp, err := ConvertImport(imp)
		if err != nil {
			return module, err
		}
		module.Imports = append(module.Imports, funImp)
	}
	// Top-level declarations
	for _, gd := range src.Decls {
		switch d := gd.(type) {
		case *ast.FuncDecl:
			fn := ConvertFuncDecl(d)
			module.Decls = append(module.Decls, fn)
		}
	}
	return module, nil
}

// ConvertImport converts Go import to Fun Import
func ConvertImport(imp *ast.ImportSpec) (fun.Import, error) {
	var result fun.Import
	var err error
	result.Path, err = litStringToString(imp.Path)
	if err != nil {
		return result, err
	}
	// TODO aliases
	return result, nil
}

// ConvertFuncDecl converts Go function declaration to the Fun one
func ConvertFuncDecl(fd *ast.FuncDecl) fun.FuncDecl {
	// Name
	fn := fun.FuncDecl{Name: identToString(fd.Name)}
	// Parameters
	if fd.Type.Params.List != nil {
		for _, p := range fd.Type.Params.List {
			tp := identToString(p.Type.(*ast.Ident))
			for _, n := range p.Names {
				fn.Params = append(fn.Params, fun.Parameter{Name: identToString(n), Type: fun.Type(tp)})
			}
		}
	}
	// Results
	if fd.Type.Results != nil {
		for _, p := range fd.Type.Results.List {
			tp := identToString(p.Type.(*ast.Ident))
			fn.Results = append(fn.Results, fun.Type(tp))
		}
	}
	// Body
	fn.Body = fun.Undefined // TODO implement body

	return fn
}

func identToString(ident *ast.Ident) string {
	return ident.Name
}

func litStringToString(lit *ast.BasicLit) (string, error) {
	switch lit.Kind {
	case token.STRING:
		return strings.Trim(lit.Value, `"`), nil
	case token.CHAR:
		return strings.Trim(lit.Value, "'"), nil
	default:
		return "", fmt.Errorf("not a string or char literal: %v", lit)
	}
}
