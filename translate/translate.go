// Package translate provides methods for convert Go AST to Fun AST.
package translate

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"strings"

	"../fun"
)

// FromFile converts Go ast.File to Fun Module.
func FromFile(fset *token.FileSet, src *ast.File) (fun.Module, error) {
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
			fn, err := ConvertFuncDecl(fset, d)
			if err != nil {
				return module, err
			}
			module.Decls = append(module.Decls, fn)
		}
	}
	return module, nil
}

// ConvertImport converts Go import to Fun Import.
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

// ConvertFuncDecl converts Go function declaration to the Fun one.
func ConvertFuncDecl(fset *token.FileSet, fd *ast.FuncDecl) (fun.FuncDecl, error) {
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
	if fd.Body == nil {
		return fn, fmt.Errorf("empty function body is not supported: %v", fd)
	}
	if len(fd.Body.List) == 1 {
		// Convert to FuncApplication
		fa, err := ConvertStmt(fset, fd.Body.List[0])
		if err != nil {
			return fn, err
		}
		fn.Body = fa
	} else {
		// Convert to Fun DoBlock
		db := fun.DoBlock{}
		for _, stmt := range fd.Body.List {
			var buf bytes.Buffer
			printer.Fprint(&buf, fset, stmt)
			db.Text = append(db.Text, buf.String())
		}
		fn.Body = db
	}

	return fn, nil
}

// ConvertStmt converts Go statement to a corresponding FuncApplication depending on type
func ConvertStmt(fset *token.FileSet, stmt ast.Stmt) (fun.FuncBody, error) {
	result := fun.FuncApplication{}
	switch st := stmt.(type) {
	case *ast.ReturnStmt:
		result.Kind = fun.EXPRESSION
		// TODO at least binary expr
		// Module:
		// Name:
		// Arguments:
		ast.Print(fset, st)       // debug
		return fun.Undefined, nil // TODO change return type to FuncApplication
	case *ast.ExprStmt:
		result.Kind = fun.STATEMENT
		switch expr := st.X.(type) {
		case *ast.CallExpr:
			switch f := expr.Fun.(type) {
			case *ast.SelectorExpr:
				result.Name = identToString(f.Sel)
				// Module (only?)
				switch x := f.X.(type) {
				case *ast.Ident:
					result.Module = identToString(x)
				default:
					ast.Print(fset, f)                                          // debug
					return result, fmt.Errorf("call type not supported: %v", f) // TODO add more
				}
			default:
				ast.Print(fset, f)                                          // debug
				return result, fmt.Errorf("call type not supported: %v", f) // TODO add more
			}
			// result.Name = f.Fun.Sel.Name
			// Arguments
			for _, e := range expr.Args {
				var arg fun.Argument
				switch a := e.(type) {
				case *ast.BasicLit:
					arg = litToArgument(a)
				default:
					ast.Print(fset, e)                                              // debug
					return result, fmt.Errorf("argument type not supported: %v", e) // TODO add more
				}
				result.Arguments = append(result.Arguments, arg)
			}
		default:
			ast.Print(fset, expr)                                              // debug
			return result, fmt.Errorf("ast.Expr type not supported: %v", expr) // TODO add more
		}
	default:
		ast.Print(fset, st)                                              // debug
		return result, fmt.Errorf("ast.Stmt type not supported: %v", st) // TODO add more
	}
	return result, nil
}

func litToArgument(lit *ast.BasicLit) fun.Argument {
	var arg fun.Argument
	switch lit.Kind {
	case token.INT:
		arg = fun.Int(lit.Value)
	case token.FLOAT:
		arg = fun.Double(lit.Value)
	case token.STRING:
		s, _ := litStringToString(lit) // should not be error
		arg = fun.String(s)
	case token.CHAR:
		s, _ := litStringToString(lit) // should not be error
		arg = fun.Char(s)
	case token.IMAG:
		arg = fun.Imaginary(lit.Value)
	default:
		panic(fmt.Sprintf("unexpected type: %v", lit))
	}
	return arg
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
